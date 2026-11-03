/// <summary>
///  StyledTaskDialog and the message hooks. No modal dialog is ever shown here.
/// </summary>
unit StyledDialogTests;

interface

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Winapi.CommCtrl,
  Vcl.Graphics,
  Vcl.Themes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.Dialogs,
  DUnitX.TestFramework,
  Vcl.StyledTaskDialog,
  Vcl.ButtonStylesAttributes,
  Vcl.StandardButtonStyles,
  Vcl.StyledButton,
  Vcl.StyledTaskDialogFormUnit, // registers the styled dialog form (without it the native dialog is used)
  StyledTestUtils;

type
  [TestFixture]
  TStyledTaskDialogSmokeTests = class
  public
    /// <summary>Sanity: a fresh dialog has no custom buttons.</summary>
    [Test]
    procedure Create_Default_HasNoCustomButtons;
  end;

  [TestFixture]
  TStyledTaskDialogLifetimeTests = class
  public
    /// <summary>
    ///  Regression (D4). The dialog form was owned by the form that launched it:
    ///  destroying that form while the dialog was modal destroyed the dialog too,
    ///  and DoExecute then freed it a second time (EInvalidPointer).
    /// </summary>
    [Test]
    procedure OwnerFormDestroyedWhileModal_DoesNotCrash;

    /// <summary>
    ///  The dialog opens on the monitor of the form that launched it (second
    ///  monitor), both with Execute(handle) and with Execute() as the demo does.
    ///  Skipped on a single monitor machine.
    /// </summary>
    [Test]
    [TestCase('ExecuteWithHandle', 'True,False')]
    [TestCase('ExecuteWithoutHandle', 'False,False')]
    [TestCase('RelativeToWindowWithHandle', 'True,True')]
    [TestCase('RelativeToWindowWithoutHandle', 'False,True')]
    procedure DialogOpensOnTheMonitorOfTheCallingForm(const AWithHandle, ARelativeToWindow: Boolean);
  end;

  [TestFixture]
  TStyledTaskDialogRadioTests = class
  public
    /// <summary>
    ///  Regression (D1). On the styled path RadioButton was only set inside the
    ///  RTL handler, which runs only when OnRadioButtonClicked is assigned: a
    ///  caller without the event read RadioButton = nil after Execute (the
    ///  native path always sets it).
    /// </summary>
    [Test]
    procedure RadioButtonClicked_WithoutEventHandler_SetsRadioButton;

    /// <summary>Guard: with the event assigned RadioButton is set as before.</summary>
    [Test]
    procedure RadioButtonClicked_WithEventHandler_SetsRadioButton;
  end;

  [TestFixture]
  TStyledTaskDialogFamilyTests = class
  public
    /// <summary>
    ///  Regression: the launcher assigned the family it received to the form even
    ///  when it was empty (a dialog without StyleFamily), erasing the default
    ///  Classic family of the form and raising an error. The buttons must use
    ///  the Classic family.
    /// </summary>
    [Test]
    procedure DialogWithoutStyleFamily_ButtonsUseTheClassicFamily;
  end;

  [TestFixture]
  TStyledTaskDialogButtonsBorderTests = class
  public
    /// <summary>DialogButtonsBorderWidth and DialogButtonsShadowSize reach every button of the dialog.</summary>
    [Test]
    procedure DialogButtonsBorderAndShadow_AreAppliedToTheButtons;
    /// <summary>The values registered with RegisterDefaultRenderingStyle are the ones of new dialogs, and are not stored.</summary>
    [Test]
    procedure RegisteredDefaults_AreTheOnesOfNewDialogs;
    /// <summary>Values below -1 are refused, like StyleBorderWidth and StyleShadowSize.</summary>
    [Test]
    procedure ValuesBelowTheDefault_AreRefused;
  end;

  [TestFixture]
  TStyledTaskDialogTextBlinkTests = class
  public
    /// <summary>With TextBlink the text of the dialog is painted in the blink colour in one phase and not in the other.</summary>
    [Test]
    procedure Text_BlinksBetweenTheTwoColours;
    /// <summary>The same under a VCL style, whose style hook forces the colour of the SysLink text.</summary>
    [Test]
    procedure Text_BlinksUnderAVclStyle;
    /// <summary>With TextBlinkLinks the links take the blink colour too: their custom colours are on in the blink phase only.</summary>
    [Test]
    procedure Links_BlinkWithTheText_WhenAsked;
    /// <summary>TextBlinkLinks is True by default.</summary>
    [Test]
    procedure TextBlinkLinks_IsTrueByDefault;
    /// <summary>With TextBlinkLinks False the links keep the colours of the system: the state of the link never changes.</summary>
    [Test]
    procedure Links_KeepTheirColours_WhenSwitchedOff;
    /// <summary>Without TextBlink the blink colour is never painted.</summary>
    [Test]
    procedure WithoutTextBlink_TheBlinkColourIsNeverPainted;
    /// <summary>The message dialogs blink under SetDialogTextBlink, and not after the previous setting is restored.</summary>
    [Test]
    procedure MessageDlg_BlinksUnderTheDefault;
    /// <summary>The defaults of SetDialogTextBlink are the ones of every new dialog.</summary>
    [Test]
    procedure Defaults_AreTheOnesOfNewDialogs;
    /// <summary>An interval below the minimum (two blinks a second at most) is raised to it.</summary>
    [Test]
    procedure IntervalBelowTheMinimum_IsRaised;
  end;

  [TestFixture]
  TStyledTaskDialogAutoClickDelayTests = class
  public
    /// <summary>
    ///  SetDialogAutoClickDelay changes only the delay: a message dialog with
    ///  only [mbOK] answers itself, and the previous value can be restored.
    /// </summary>
    [Test]
    procedure MessageDlgUnderTheDelay_AnswersItself;

    /// <summary>A dialog created under the delay has AutoClick, one created after restoring it has not.</summary>
    [Test]
    procedure DialogCreatedUnderTheDelay_HasAutoClick;
  end;

implementation

type
  /// <summary>Watchdog: closes the dialog if it did not answer itself.</summary>
  TWatchdog = class
  public
    Timer: TTimer;
    Fired: Boolean;
    procedure OnTimer(Sender: TObject);
  end;

  /// <summary>Reads the family of the buttons while the dialog is modal, then closes it.</summary>
  TFamilyProbe = class
  public
    Timer: TTimer;
    Found: Boolean;
    ButtonFamily: TStyledButtonFamily;
    CustomButtonFamily: TStyledButtonFamily;
    ButtonBorderWidth, ButtonShadowSize: Integer;
    CustomBorderWidth, CustomShadowSize: Integer;
    procedure OnTimer(Sender: TObject);
  end;

  /// <summary>
  ///  Samples the text colour of the shown dialog: the answer of the text label to
  ///  WM_CTLCOLORSTATIC, which is what a SysLink paints its text with (clRed is the
  ///  blink colour of the tests).
  /// </summary>
  TBlinkProbe = class
  public
    Timer: TTimer;
    Samples: Integer;
    SeenBlinking: Boolean;
    SeenNormal: Boolean;
    //state of the first link of the text: with or without LIS_DEFAULTCOLORS
    SeenLinkWithCustomColours: Boolean;
    SeenLinkWithDefaultColours: Boolean;
    procedure OnTimer(Sender: TObject);
  end;

  /// <summary>Destroys the form that launched the dialog while it is modal, then closes it.</summary>
  TOwnerKiller = class
  public
    Owner: TForm;
    Step: Integer;
    Timer: TTimer;
    DialogMonitor: Integer;
    DialogLeft: Integer;
    CloseOnly: Boolean;
    procedure OnTimer(Sender: TObject);
  end;

  TRadioProbe = class
  public
    Count: Integer;
    procedure Clicked(Sender: TObject);
  end;

procedure TBlinkProbe.OnTimer(Sender: TObject);
const
  MAX_SAMPLES = 24; //70 ms each: enough to see both phases at the minimum interval
var
  I: Integer;
  LForm: TStyledTaskDialogForm;
  LDC: HDC;
  LItem: TLItem;
begin
  Inc(Samples);
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I] is TStyledTaskDialogForm then
    begin
      LForm := TStyledTaskDialogForm(Screen.Forms[I]);
      //The SysLink paints the text with the colour of the reply to WM_CTLCOLORSTATIC,
      //not with Font.Color: ask the same question to the label and read the answer
      LDC := CreateCompatibleDC(0);
      try
        LForm.TextLabel.Perform(CN_CTLCOLORSTATIC, WPARAM(LDC), LPARAM(LForm.TextLabel.Handle));
        if GetTextColor(LDC) = COLORREF(ColorToRGB(clRed)) then
          SeenBlinking := True
        else
          SeenNormal := True;
      finally
        DeleteDC(LDC);
      end;
      FillChar(LItem, SizeOf(LItem), 0);
      LItem.mask := LIF_ITEMINDEX or LIF_STATE;
      LItem.iLink := 0;
      LItem.stateMask := LIS_DEFAULTCOLORS;
      if LForm.TextLabel.Perform(LM_GETITEM, 0, LPARAM(@LItem)) <> 0 then
      begin
        //LIS_DEFAULTCOLORS ON enables the custom colours ("enable custom text colors")
        if (LItem.state and LIS_DEFAULTCOLORS) <> 0 then
          SeenLinkWithCustomColours := True
        else
          SeenLinkWithDefaultColours := True;
      end;
      if Samples >= MAX_SAMPLES then
      begin
        Timer.Enabled := False;
        LForm.ModalResult := mrOk;
      end;
      Break;
    end;
end;

procedure TFamilyProbe.OnTimer(Sender: TObject);
var
  I: Integer;
  J: Integer;
  LForm: TStyledTaskDialogForm;
begin
  Timer.Enabled := False;
  for I := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[I] is TStyledTaskDialogForm then
    begin
      LForm := TStyledTaskDialogForm(Screen.Forms[I]);
      Found := True;
      ButtonFamily := LForm.OKButton.StyleFamily;
      ButtonBorderWidth := LForm.OKButton.StyleBorderWidth;
      ButtonShadowSize := LForm.OKButton.StyleShadowSize;
      // the button created by the form for the custom item of the dialog
      for J := 0 to LForm.ComponentCount - 1 do
        if (LForm.Components[J] is TStyledButton) and
          (TStyledButton(LForm.Components[J]).Caption = 'Custom') then
        begin
          CustomButtonFamily := TStyledButton(LForm.Components[J]).StyleFamily;
          CustomBorderWidth := TStyledButton(LForm.Components[J]).StyleBorderWidth;
          CustomShadowSize := TStyledButton(LForm.Components[J]).StyleShadowSize;
        end;
      LForm.ModalResult := mrOk;
      Break;
    end;
  end;
end;

procedure TWatchdog.OnTimer(Sender: TObject);
var
  I: Integer;
begin
  Timer.Enabled := False;
  Fired := True;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I] is TStyledTaskDialogForm then
    begin
      Screen.Forms[I].ModalResult := mrCancel;
      Break;
    end;
end;

procedure TRadioProbe.Clicked(Sender: TObject);
begin
  Inc(Count);
end;

procedure TOwnerKiller.OnTimer(Sender: TObject);
var
  I: Integer;
begin
  Inc(Step);
  if (Step = 1) and not CloseOnly then
  begin
    // the form that launched the dialog goes away while the dialog is modal
    FreeAndNil(Owner);
  end
  else
  begin
    Timer.Enabled := False;
    for I := 0 to Screen.FormCount - 1 do
    begin
      if Screen.Forms[I] is TStyledTaskDialogForm then
      begin
        DialogMonitor := Screen.Forms[I].Monitor.MonitorNum;
        DialogLeft := Screen.Forms[I].Left;
        Screen.Forms[I].ModalResult := mrOk;
        Break;
      end;
    end;
  end;
end;

{ TStyledTaskDialogSmokeTests }

procedure TStyledTaskDialogSmokeTests.Create_Default_HasNoCustomButtons;
var
  LDialog: TStyledTaskDialog;
begin
  LDialog := TStyledTaskDialog.Create(nil);
  try
    Assert.AreEqual(0, LDialog.Buttons.Count, 'Buttons.Count');
  finally
    LDialog.Free;
  end;
end;

{ TStyledTaskDialogLifetimeTests }

procedure TStyledTaskDialogLifetimeTests.OwnerFormDestroyedWhileModal_DoesNotCrash;
var
  LKiller: TOwnerKiller;
  LDialog: TStyledTaskDialog;
begin
  LKiller := TOwnerKiller.Create;
  LDialog := TStyledTaskDialog.Create(nil);
  try
    LKiller.Owner := TForm.CreateNew(nil);
    LKiller.Owner.SetBounds(-32000, -32000, 300, 200);
    LKiller.Owner.Show;
    LKiller.Timer := TTimer.Create(nil);
    LKiller.Timer.Interval := 150;
    LKiller.Timer.OnTimer := LKiller.OnTimer;
    LKiller.Timer.Enabled := True;
    LDialog.Title := 'Owner destruction test';
    LDialog.Text := 'The form that launched this dialog is destroyed while it is open';
    LDialog.CommonButtons := [tcbOk];
    // a modal dialog launched from the form that the timer destroys
    LDialog.Execute(LKiller.Owner.Handle);
    Assert.IsNull(LKiller.Owner, 'the launching form was destroyed during the dialog');
  finally
    LKiller.Timer.Free;
    LKiller.Owner.Free;
    LDialog.Free;
    LKiller.Free;
  end;
end;

procedure TStyledTaskDialogLifetimeTests.DialogOpensOnTheMonitorOfTheCallingForm(const AWithHandle, ARelativeToWindow: Boolean);
var
  LKiller: TOwnerKiller;
  LDialog: TStyledTaskDialog;
  LMonitor: TMonitor;
  LMainForm: TForm;
  I: Integer;
begin
  if Screen.MonitorCount < 2 then
    Assert.Pass('single monitor: test skipped');
  //the calling form goes on a monitor that is not the primary one
  LMonitor := nil;
  for I := 0 to Screen.MonitorCount - 1 do
    if not Screen.Monitors[I].Primary then
    begin
      LMonitor := Screen.Monitors[I];
      Break;
    end;
  Assert.IsNotNull(LMonitor, 'a non primary monitor');
  LKiller := TOwnerKiller.Create;
  LDialog := TStyledTaskDialog.Create(nil);
  LMainForm := nil;
  try
    // like an application: a main form on the primary monitor, the calling form on the second
    if not Assigned(Application.MainForm) then
    begin
      Application.CreateForm(TForm, LMainForm);
      LMainForm.Position := poDesigned;
      LMainForm.SetBounds(Screen.PrimaryMonitor.Left + 50, Screen.PrimaryMonitor.Top + 50, 400, 300);
      LMainForm.Show;
    end;
    LKiller.CloseOnly := True;
    LKiller.DialogMonitor := -1;
    LKiller.Owner := TForm.CreateNew(nil);
    LKiller.Owner.Position := poDesigned;
    LKiller.Owner.SetBounds(LMonitor.Left + 100, LMonitor.Top + 100, 400, 300);
    LKiller.Owner.Show;
    LKiller.Owner.SetFocus;
    // after Show: the position the form really has
    LKiller.Owner.SetBounds(LMonitor.Left + 100, LMonitor.Top + 100, 400, 300);
    Assert.IsTrue(Screen.MonitorFromWindow(LKiller.Owner.Handle).MonitorNum = LMonitor.MonitorNum,
      Format('precondition: the calling form is on monitor %d, left=%d (monitor left=%d)',
      [LMonitor.MonitorNum, LKiller.Owner.Left, LMonitor.Left]));
    LKiller.Timer := TTimer.Create(nil);
    LKiller.Timer.Interval := 300;
    LKiller.Timer.OnTimer := LKiller.OnTimer;
    LKiller.Timer.Enabled := True;
    LDialog.Title := 'Monitor test';
    LDialog.Text := 'This dialog must open on the monitor of the calling form';
    LDialog.CommonButtons := [tcbOk];
    if ARelativeToWindow then
      LDialog.Flags := LDialog.Flags + [tfPositionRelativeToWindow];
    if AWithHandle then
      LDialog.Execute(LKiller.Owner.Handle)
    else
      LDialog.Execute;
    Assert.AreEqual(LMonitor.MonitorNum, LKiller.DialogMonitor,
      Format('dialog on monitor %d (left=%d), the calling form is on monitor %d (left=%d); monitors: %d=[%d..%d] primary=%d',
      [LKiller.DialogMonitor, LKiller.DialogLeft, LMonitor.MonitorNum, LKiller.Owner.Left,
       LMonitor.MonitorNum, LMonitor.Left, LMonitor.Left + LMonitor.Width, Screen.PrimaryMonitor.MonitorNum]));
  finally
    LKiller.Timer.Free;
    LKiller.Owner.Free;
    LMainForm.Free;
    LDialog.Free;
    LKiller.Free;
  end;
end;

{ TStyledTaskDialogRadioTests }

procedure TStyledTaskDialogRadioTests.RadioButtonClicked_WithoutEventHandler_SetsRadioButton;
var
  LDialog: TStyledTaskDialog;
  LFirst, LSecond: TTaskDialogRadioButtonItem;
begin
  LDialog := TStyledTaskDialog.Create(nil);
  try
    LFirst := LDialog.RadioButtons.Add as TTaskDialogRadioButtonItem;
    LFirst.Caption := 'One';
    LSecond := LDialog.RadioButtons.Add as TTaskDialogRadioButtonItem;
    LSecond.Caption := 'Two';
    Assert.IsFalse(Assigned(LDialog.OnRadioButtonClicked), 'precondition: no handler');
    // What the styled form does when the user picks the second radio
    LDialog.DoOnRadioButtonClicked(LSecond.ModalResult);
    Assert.IsNotNull(LDialog.RadioButton, 'RadioButton must be set without an event handler');
    Assert.AreSame(LSecond, LDialog.RadioButton, 'RadioButton must be the clicked item');
  finally
    LDialog.Free;
  end;
end;

procedure TStyledTaskDialogRadioTests.RadioButtonClicked_WithEventHandler_SetsRadioButton;
var
  LDialog: TStyledTaskDialog;
  LProbe: TRadioProbe;
  LItem: TTaskDialogRadioButtonItem;
begin
  LDialog := TStyledTaskDialog.Create(nil);
  LProbe := TRadioProbe.Create;
  try
    LItem := LDialog.RadioButtons.Add as TTaskDialogRadioButtonItem;
    LItem.Caption := 'One';
    LDialog.OnRadioButtonClicked := LProbe.Clicked;
    LDialog.DoOnRadioButtonClicked(LItem.ModalResult);
    Assert.AreEqual(1, LProbe.Count, 'event fired once');
    Assert.AreSame(LItem, LDialog.RadioButton, 'RadioButton set with the event handler');
  finally
    LProbe.Free;
    LDialog.Free;
  end;
end;

{ TStyledTaskDialogFamilyTests }

procedure TStyledTaskDialogFamilyTests.DialogWithoutStyleFamily_ButtonsUseTheClassicFamily;
var
  LProbe: TFamilyProbe;
  LDialog: TStyledTaskDialog;
  LLauncher: TTaskDialogLauncherHandler;
begin
  LProbe := TFamilyProbe.Create;
  LDialog := TStyledTaskDialog.Create(nil);
  LLauncher := TTaskDialogLauncherHandler.Create;
  try
    LProbe.Timer := TTimer.Create(nil);
    LProbe.Timer.Interval := 200;
    LProbe.Timer.OnTimer := LProbe.OnTimer;
    LProbe.Timer.Enabled := True;
    LDialog.Title := 'Family test';
    LDialog.Text := 'A dialog without StyleFamily';
    LDialog.CommonButtons := [tcbOk];
    // a custom button: the form creates a new styled button for it
    (LDialog.Buttons.Add as TTaskDialogButtonItem).Caption := 'Custom';
    // the empty family is what a caller without StyleFamily passes
    LLauncher.DoExecute(0, TMsgDlgType.mtInformation, LDialog, '');
    Assert.IsTrue(LProbe.Found, 'the styled dialog form was shown');
    Assert.AreEqual(DEFAULT_CLASSIC_FAMILY, LProbe.ButtonFamily, 'family of the OK button');
    Assert.AreEqual(DEFAULT_CLASSIC_FAMILY, LProbe.CustomButtonFamily, 'family of the custom button');
  finally
    LProbe.Timer.Free;
    LDialog.Free;
    LProbe.Free;
  end;
end;

{ TStyledTaskDialogButtonsBorderTests }

procedure TStyledTaskDialogButtonsBorderTests.DialogButtonsBorderAndShadow_AreAppliedToTheButtons;
var
  LProbe: TFamilyProbe;
  LDialog: TStyledTaskDialog;
  LLauncher: TTaskDialogLauncherHandler;
begin
  LProbe := TFamilyProbe.Create;
  LDialog := TStyledTaskDialog.Create(nil);
  LLauncher := TTaskDialogLauncherHandler.Create;
  try
    LProbe.Timer := TTimer.Create(nil);
    LProbe.Timer.Interval := 200;
    LProbe.Timer.OnTimer := LProbe.OnTimer;
    LProbe.Timer.Enabled := True;
    LDialog.Title := 'Border test';
    LDialog.Text := 'Buttons with their own border and shadow';
    LDialog.CommonButtons := [tcbOk];
    (LDialog.Buttons.Add as TTaskDialogButtonItem).Caption := 'Custom';
    LDialog.DialogButtonsBorderWidth := 4;
    LDialog.DialogButtonsShadowSize := 3;
    LLauncher.DoExecute(0, TMsgDlgType.mtInformation, LDialog, '');
    Assert.IsTrue(LProbe.Found, 'the styled dialog form was shown');
    Assert.AreEqual(4, LProbe.ButtonBorderWidth, 'border width of the OK button');
    Assert.AreEqual(3, LProbe.ButtonShadowSize, 'shadow of the OK button');
    Assert.AreEqual(4, LProbe.CustomBorderWidth, 'border width of the custom button');
    Assert.AreEqual(3, LProbe.CustomShadowSize, 'shadow of the custom button');
  finally
    LProbe.Timer.Free;
    LDialog.Free;
    LProbe.Free;
  end;
end;

procedure TStyledTaskDialogButtonsBorderTests.RegisteredDefaults_AreTheOnesOfNewDialogs;
var
  LDialog: TStyledTaskDialog;
begin
  TStyledTaskDialog.RegisterDefaultRenderingStyle(btRoundRect, DEFAULT_CLASSIC_FAMILY,
    DEFAULT_RADIUS, ALL_ROUNDED_CORNERS, 2, 5);
  try
    LDialog := TStyledTaskDialog.Create(nil);
    try
      Assert.AreEqual(2, LDialog.DialogButtonsShadowSize, 'shadow');
      Assert.AreEqual(5, LDialog.DialogButtonsBorderWidth, 'border width');
    finally
      LDialog.Free;
    end;
  finally
    TStyledTaskDialog.RegisterDefaultRenderingStyle(btRoundRect, DEFAULT_CLASSIC_FAMILY,
      DEFAULT_RADIUS, ALL_ROUNDED_CORNERS, DEFAULT_SHADOW_SIZE, DEFAULT_BORDER_WIDTH);
  end;
  LDialog := TStyledTaskDialog.Create(nil);
  try
    Assert.AreEqual(DEFAULT_SHADOW_SIZE, LDialog.DialogButtonsShadowSize, 'shadow restored');
    Assert.AreEqual(DEFAULT_BORDER_WIDTH, LDialog.DialogButtonsBorderWidth, 'border width restored');
    Assert.IsTrue(Pos('DialogButtonsBorderWidth', TStyledTestUtils.DfmText(LDialog)) = 0, 'the default is not stored');
    Assert.IsTrue(Pos('DialogButtonsShadowSize', TStyledTestUtils.DfmText(LDialog)) = 0, 'the default is not stored');
    LDialog.DialogButtonsBorderWidth := 3;
    LDialog.DialogButtonsShadowSize := 1;
    Assert.IsTrue(Pos('DialogButtonsBorderWidth = 3', TStyledTestUtils.DfmText(LDialog)) > 0, 'stored when different');
    Assert.IsTrue(Pos('DialogButtonsShadowSize = 1', TStyledTestUtils.DfmText(LDialog)) > 0, 'stored when different');
  finally
    LDialog.Free;
  end;
end;

procedure TStyledTaskDialogButtonsBorderTests.ValuesBelowTheDefault_AreRefused;
var
  LDialog: TStyledTaskDialog;
  LBorderRaised, LShadowRaised, LRegisterRaised: Boolean;
begin
  LDialog := TStyledTaskDialog.Create(nil);
  try
    LBorderRaised := False;
    LShadowRaised := False;
    LRegisterRaised := False;
    try
      LDialog.DialogButtonsBorderWidth := -2;
    except
      on EReadError do LBorderRaised := True;
    end;
    try
      LDialog.DialogButtonsShadowSize := -2;
    except
      on EReadError do LShadowRaised := True;
    end;
    try
      TStyledTaskDialog.RegisterDefaultRenderingStyle(btRoundRect, DEFAULT_CLASSIC_FAMILY,
        DEFAULT_RADIUS, ALL_ROUNDED_CORNERS, -2, DEFAULT_BORDER_WIDTH);
    except
      on EArgumentException do LRegisterRaised := True;
    end;
    Assert.IsTrue(LBorderRaised, 'DialogButtonsBorderWidth');
    Assert.IsTrue(LShadowRaised, 'DialogButtonsShadowSize');
    Assert.IsTrue(LRegisterRaised, 'RegisterDefaultRenderingStyle');
  finally
    LDialog.Free;
  end;
end;

{ TStyledTaskDialogTextBlinkTests }

//Shows a dialog with the text blinking (or not) and returns what the probe saw
procedure ShowAndSample(const ABlink: Boolean; const AProbe: TBlinkProbe;
  const ALinks: Boolean = True);
var
  LDialog: TStyledTaskDialog;
  LLauncher: TTaskDialogLauncherHandler;
begin
  LDialog := TStyledTaskDialog.Create(nil);
  LLauncher := TTaskDialogLauncherHandler.Create;
  try
    AProbe.Timer := TTimer.Create(nil);
    try
      AProbe.Timer.Interval := 70;
      AProbe.Timer.OnTimer := AProbe.OnTimer;
      AProbe.Timer.Enabled := True;
      LDialog.Title := 'Blink test';
      LDialog.Text := 'A text that blinks: <a href="https://ethea.it">a link</a> WWWWWWWWWWWWWWWWWWWW';
      LDialog.CommonButtons := [tcbOk];
      LDialog.TextBlink := ABlink;
      LDialog.TextBlinkLinks := ALinks;
      LDialog.TextBlinkColor := clRed;
      LDialog.TextBlinkInterval := MIN_TEXTBLINK_INTERVAL;
      LLauncher.DoExecute(0, TMsgDlgType.mtInformation, LDialog, '');
    finally
      AProbe.Timer.Free;
    end;
  finally
    LLauncher.Free;
    LDialog.Free;
  end;
end;

procedure TStyledTaskDialogTextBlinkTests.Text_BlinksBetweenTheTwoColours;
var
  LProbe: TBlinkProbe;
begin
  LProbe := TBlinkProbe.Create;
  try
    ShowAndSample(True, LProbe);
    Assert.IsTrue(LProbe.SeenBlinking, 'the text was painted in the blink colour');
    Assert.IsTrue(LProbe.SeenNormal, 'the text was painted without it in the other phase');
  finally
    LProbe.Free;
  end;
end;

procedure TStyledTaskDialogTextBlinkTests.Text_BlinksUnderAVclStyle;
const
  STYLE_NAME = 'Carbon';
var
  LStyleFile: string;
  LProbe: TBlinkProbe;
begin
  LStyleFile := IncludeTrailingPathDelimiter(GetEnvironmentVariable('BDS')) +
    'Redist\styles\vcl\' + STYLE_NAME + '.vsf';
  if not FileExists(LStyleFile) then
    LStyleFile := 'C:\BDS\Studio\37.0\Redist\styles\vcl\' + STYLE_NAME + '.vsf';
  if not FileExists(LStyleFile) then
    Assert.Pass(STYLE_NAME + '.vsf not found: VCL style test skipped');
  if TStyleManager.Style[STYLE_NAME] = nil then
    TStyleManager.LoadFromFile(LStyleFile);
  Assert.IsTrue(TStyleManager.TrySetStyle(STYLE_NAME, False), 'TrySetStyle ' + STYLE_NAME);
  try
    LProbe := TBlinkProbe.Create;
    try
      ShowAndSample(True, LProbe);
      Assert.IsTrue(LProbe.SeenBlinking, 'the text was painted in the blink colour under ' + STYLE_NAME);
      Assert.IsTrue(LProbe.SeenNormal, 'and without it in the other phase');
    finally
      LProbe.Free;
    end;
  finally
    TStyleManager.SetStyle(TStyleManager.SystemStyle);
  end;
end;

procedure TStyledTaskDialogTextBlinkTests.Links_BlinkWithTheText_WhenAsked;
var
  LProbe: TBlinkProbe;
begin
  LProbe := TBlinkProbe.Create;
  try
    ShowAndSample(True, LProbe);
    Assert.IsTrue(LProbe.SeenLinkWithCustomColours, 'the link used the colour of the text in the blink phase');
    Assert.IsTrue(LProbe.SeenLinkWithDefaultColours, 'and its own colours in the other phase');
  finally
    LProbe.Free;
  end;
end;

procedure TStyledTaskDialogTextBlinkTests.TextBlinkLinks_IsTrueByDefault;
var
  LDialog: TStyledTaskDialog;
begin
  LDialog := TStyledTaskDialog.Create(nil);
  try
    Assert.IsTrue(LDialog.TextBlinkLinks);
  finally
    LDialog.Free;
  end;
end;

procedure TStyledTaskDialogTextBlinkTests.Links_KeepTheirColours_WhenSwitchedOff;
var
  LProbe: TBlinkProbe;
begin
  LProbe := TBlinkProbe.Create;
  try
    ShowAndSample(True, LProbe, False);
    Assert.IsTrue(LProbe.SeenBlinking, 'the text blinked');
    //"not both" is also met by a probe that never read the link, or by a flag stuck on
    Assert.IsTrue(LProbe.SeenLinkWithDefaultColours, 'the link was read, with its own colours');
    Assert.IsFalse(LProbe.SeenLinkWithCustomColours, 'the link never took the custom (blink) colours');
  finally
    LProbe.Free;
  end;
end;

procedure TStyledTaskDialogTextBlinkTests.WithoutTextBlink_TheBlinkColourIsNeverPainted;
var
  LProbe: TBlinkProbe;
begin
  LProbe := TBlinkProbe.Create;
  try
    ShowAndSample(False, LProbe);
    Assert.IsFalse(LProbe.SeenBlinking, 'the blink colour was never painted');
    Assert.IsTrue(LProbe.SeenNormal, 'the dialog was sampled');
  finally
    LProbe.Free;
  end;
end;

procedure TStyledTaskDialogTextBlinkTests.MessageDlg_BlinksUnderTheDefault;
var
  LProbe: TBlinkProbe;
  LPreviousBlink: Boolean;
  LPreviousColor: TColor;
  LPreviousInterval: Integer;
begin
  LPreviousBlink := GetDialogTextBlink;
  LPreviousColor := GetDialogTextBlinkColor;
  LPreviousInterval := GetDialogTextBlinkInterval;
  LProbe := TBlinkProbe.Create;
  try
    LProbe.Timer := TTimer.Create(nil);
    try
      LProbe.Timer.Interval := 70;
      LProbe.Timer.OnTimer := LProbe.OnTimer;
      SetDialogTextBlink(True, clRed, MIN_TEXTBLINK_INTERVAL);
      LProbe.Timer.Enabled := True;
      StyledTaskMessageDlg('Blink test', 'A message that blinks: WWWWWWWWWWWWWWWWWWWW',
        TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
    finally
      SetDialogTextBlink(LPreviousBlink, LPreviousColor, LPreviousInterval);
      LProbe.Timer.Free;
    end;
    Assert.IsTrue(LProbe.SeenBlinking, 'the message blinked');
    Assert.IsTrue(LProbe.SeenNormal, 'in both phases');
  finally
    LProbe.Free;
  end;
  Assert.AreEqual(LPreviousBlink, GetDialogTextBlink, 'the previous setting is restored');
  Assert.AreEqual(LPreviousInterval, GetDialogTextBlinkInterval, 'the previous interval is restored');
end;

procedure TStyledTaskDialogTextBlinkTests.Defaults_AreTheOnesOfNewDialogs;
var
  LDialog: TStyledTaskDialog;
  LPreviousBlink: Boolean;
  LPreviousColor: TColor;
  LPreviousInterval: Integer;
begin
  LDialog := TStyledTaskDialog.Create(nil);
  try
    Assert.IsFalse(LDialog.TextBlink, 'off by default');
    Assert.AreEqual(Integer(DEFAULT_TEXTBLINK_COLOR), Integer(LDialog.TextBlinkColor), 'default colour');
    Assert.AreEqual(DEFAULT_TEXTBLINK_INTERVAL, LDialog.TextBlinkInterval, 'default interval');
    Assert.IsTrue(Pos('TextBlink', TStyledTestUtils.DfmText(LDialog)) = 0, 'the defaults are not stored');
  finally
    LDialog.Free;
  end;
  LPreviousBlink := GetDialogTextBlink;
  LPreviousColor := GetDialogTextBlinkColor;
  LPreviousInterval := GetDialogTextBlinkInterval;
  SetDialogTextBlink(True, clLime, 400);
  try
    LDialog := TStyledTaskDialog.Create(nil);
    try
      Assert.IsTrue(LDialog.TextBlink, 'blink');
      Assert.AreEqual(Integer(clLime), Integer(LDialog.TextBlinkColor), 'colour');
      Assert.AreEqual(400, LDialog.TextBlinkInterval, 'interval');
    finally
      LDialog.Free;
    end;
  finally
    SetDialogTextBlink(LPreviousBlink, LPreviousColor, LPreviousInterval);
  end;
  Assert.AreEqual(LPreviousBlink, GetDialogTextBlink, 'restored');
end;

procedure TStyledTaskDialogTextBlinkTests.IntervalBelowTheMinimum_IsRaised;
var
  LDialog: TStyledTaskDialog;
  LPreviousBlink: Boolean;
  LPreviousColor: TColor;
  LPreviousInterval: Integer;
begin
  LDialog := TStyledTaskDialog.Create(nil);
  try
    LDialog.TextBlinkInterval := 10;
    Assert.AreEqual(MIN_TEXTBLINK_INTERVAL, LDialog.TextBlinkInterval, 'through the property');
  finally
    LDialog.Free;
  end;
  LPreviousBlink := GetDialogTextBlink;
  LPreviousColor := GetDialogTextBlinkColor;
  LPreviousInterval := GetDialogTextBlinkInterval;
  SetDialogTextBlink(True, clRed, 10);
  try
    Assert.AreEqual(MIN_TEXTBLINK_INTERVAL, GetDialogTextBlinkInterval, 'through SetDialogTextBlink');
    LDialog := TStyledTaskDialog.Create(nil);
    try
      Assert.AreEqual(MIN_TEXTBLINK_INTERVAL, LDialog.TextBlinkInterval, 'new dialogs');
    finally
      LDialog.Free;
    end;
  finally
    SetDialogTextBlink(LPreviousBlink, LPreviousColor, LPreviousInterval);
  end;
end;

{ TStyledTaskDialogAutoClickDelayTests }

procedure TStyledTaskDialogAutoClickDelayTests.MessageDlgUnderTheDelay_AnswersItself;
var
  LWatchdog: TWatchdog;
  LPrevious, LResult: Integer;
begin
  LWatchdog := TWatchdog.Create;
  LPrevious := GetDialogAutoClickDelay;
  try
    LWatchdog.Timer := TTimer.Create(nil);
    LWatchdog.Timer.Interval := 5000;
    LWatchdog.Timer.OnTimer := LWatchdog.OnTimer;
    SetDialogAutoClickDelay(500);
    LWatchdog.Timer.Enabled := True;
    LResult := StyledTaskMessageDlg('Delay test', 'Nobody is at the keyboard',
      TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
    Assert.AreEqual(Integer(mrOk), LResult, 'the dialog pressed OK itself');
    Assert.IsFalse(LWatchdog.Fired, 'the watchdog did not fire');
  finally
    SetDialogAutoClickDelay(LPrevious);
    LWatchdog.Timer.Free;
    LWatchdog.Free;
  end;
  Assert.AreEqual(LPrevious, GetDialogAutoClickDelay, 'the delay is restored');
end;

procedure TStyledTaskDialogAutoClickDelayTests.DialogCreatedUnderTheDelay_HasAutoClick;
var
  LPrevious: Integer;
  LDialog: TStyledTaskDialog;
begin
  LPrevious := GetDialogAutoClickDelay;
  try
    SetDialogAutoClickDelay(3000);
    Assert.AreEqual(3000, GetDialogAutoClickDelay, 'getter');
    LDialog := TStyledTaskDialog.Create(nil);
    try
      Assert.IsTrue(LDialog.AutoClick, 'AutoClick under the delay');
      Assert.AreEqual(3000, LDialog.AutoClickDelay, 'AutoClickDelay');
    finally
      LDialog.Free;
    end;
    SetDialogAutoClickDelay(0);
    LDialog := TStyledTaskDialog.Create(nil);
    try
      Assert.IsFalse(LDialog.AutoClick, 'no AutoClick after the delay is cleared');
    finally
      LDialog.Free;
    end;
  finally
    SetDialogAutoClickDelay(LPrevious);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TStyledTaskDialogSmokeTests);
  TDUnitX.RegisterTestFixture(TStyledTaskDialogLifetimeTests);
  TDUnitX.RegisterTestFixture(TStyledTaskDialogRadioTests);
  TDUnitX.RegisterTestFixture(TStyledTaskDialogFamilyTests);
  TDUnitX.RegisterTestFixture(TStyledTaskDialogButtonsBorderTests);
  TDUnitX.RegisterTestFixture(TStyledTaskDialogTextBlinkTests);
  TDUnitX.RegisterTestFixture(TStyledTaskDialogAutoClickDelayTests);

end.
