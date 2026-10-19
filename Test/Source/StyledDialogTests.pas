/// <summary>
///  StyledTaskDialog and the message hooks. No modal dialog is ever shown here.
/// </summary>
unit StyledDialogTests;

interface

uses
  System.SysUtils,
  System.Classes,
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
      // the button created by the form for the custom item of the dialog
      for J := 0 to LForm.ComponentCount - 1 do
        if (LForm.Components[J] is TStyledButton) and
          (TStyledButton(LForm.Components[J]).Caption = 'Custom') then
          CustomButtonFamily := TStyledButton(LForm.Components[J]).StyleFamily;
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
  TDUnitX.RegisterTestFixture(TStyledTaskDialogAutoClickDelayTests);

end.
