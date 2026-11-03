/// <summary>
///  The button components: TStyledGraphicButton, TStyledButton, TStyledSpeedButton, TStyledBitBtn.
/// </summary>
unit StyledButtonTests;

interface

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  DUnitX.TestFramework,
  Vcl.ButtonStylesAttributes,
  Vcl.BootstrapButtonStyles,
  Vcl.AngularButtonStyles,
  Vcl.StyledButton,
  Vcl.StyledPanel,
  StyledTestUtils;

type
  [TestFixture]
  TStyledButtonSmokeTests = class
  public
    /// <summary>Sanity: a default TStyledButton is born with a registered StyleFamily.</summary>
    [Test]
    procedure Create_Default_HasRegisteredFamily;
  end;

  [TestFixture]
  TStyledButtonAssignTests = class
  public
    /// <summary>
    ///  Regression (B1). AssignTo called inherited AssignTo before copying:
    ///  TControl.AssignTo only handles TCustomAction and otherwise raises
    ///  AssignError, so Dest.Assign(Source) between two styled graphic buttons
    ///  always raised EConvertError and the copy code was unreachable.
    /// </summary>
    [Test]
    procedure Assign_GraphicButton_CopiesStyleAndCaption;

    /// <summary>Regression (B1). Same defect in TCustomStyledButton.AssignTo.</summary>
    [Test]
    procedure Assign_WindowedButton_CopiesStyleAndCaption;

    /// <summary>Guard: assigning a button to a TAction (the inherited use of AssignTo) still works.</summary>
    [Test]
    procedure AssignTo_Action_StillWorks;
  end;

  [TestFixture]
  TStyledButtonStyleTests = class
  public
    /// <summary>
    ///  Regression (R2/B3). Setting StyleFamily alone on a Classic/Windows button
    ///  made ApplyButtonStyle adopt the new family's default class and appearance
    ///  but skip StyleFamilyUpdateAttributes, so the button kept the Classic
    ///  colours while StyleClass reported 'Primary'.
    /// </summary>
    [Test]
    procedure StyleFamilyChangedAlone_RecomputesTheAttributes;

    /// <summary>
    ///  Changing the family to Angular-M3 resets the class and the appearance that
    ///  the new family does not have (Bootstrap Danger/Outline) to its defaults
    ///  (Basic/Filled) and recomputes the colours.
    /// </summary>
    [Test]
    procedure FamilyChangeToM3_ResetsUnknownClassAndAppearance;

    /// <summary>
    ///  The class and the appearance that the new family also has are kept:
    ///  Angular-Light Basic/Raised -> M3 keeps Basic but Raised becomes Filled;
    ///  Light -> Dark keeps the M3 appearance.
    /// </summary>
    [Test]
    procedure FamilyChangeToM3_KeepsTheValidValues;

    /// <summary>From M3 (Basic/Tonal) back to Bootstrap: Bootstrap defaults and Bootstrap colours.</summary>
    [Test]
    procedure FamilyChangeFromM3_ResetsToTheNewFamilyDefaults;

    /// <summary>The same reset happens in the TStyledPanel.</summary>
    [Test]
    procedure PanelFamilyChange_ResetsClassAndAppearance;
  end;

  [TestFixture]
  TStyledButtonKeyboardTests = class
  private
    FClicks: Integer;
    procedure CountClick(Sender: TObject);
  public
    /// <summary>
    ///  Regression (D2). CNKeyDown forced Result := 0 after the inherited call,
    ///  so a dialog key handled by CMDialogKey (which clicked the button) was
    ///  reported as unhandled and the WM_KEYDOWN reached a KeyPreview form,
    ///  which clicked the button a second time.
    /// </summary>
    [Test]
    procedure CNKeyDown_DialogKeyHandled_ReturnsOneAndClicksOnce;
    /// <summary>
    ///  CMDialogKey clicks only with no modifier down, read from the thread's keyboard state
    ///  (KeyDataToShiftState -> GetKeyState): with Shift down Esc must not click.
    /// </summary>
    [Test]
    procedure CNKeyDown_WithShiftDown_DoesNotClick;
  end;

implementation

uses
  System.Actions,
  Vcl.ActnList;

{ TStyledButtonSmokeTests }

procedure TStyledButtonSmokeTests.Create_Default_HasRegisteredFamily;
var
  LButton: TStyledButton;
begin
  LButton := TStyledButton.Create(nil);
  try
    Assert.IsTrue(StyleFamilyExists(LButton.StyleFamily), 'family: ' + LButton.StyleFamily);
  finally
    LButton.Free;
  end;
end;

{ TStyledButtonAssignTests }

procedure TStyledButtonAssignTests.Assign_GraphicButton_CopiesStyleAndCaption;
var
  LSource, LDest: TStyledGraphicButton;
begin
  LSource := TStyledGraphicButton.Create(nil);
  LDest := TStyledGraphicButton.Create(nil);
  try
    LSource.StyleFamily := BOOTSTRAP_FAMILY;
    LSource.StyleClass := btn_danger;
    LSource.StyleAppearance := BOOTSTRAP_OUTLINE;
    LSource.Caption := 'Source';
    LSource.Hint := 'Source hint';
    LDest.Assign(LSource);
    Assert.AreEqual(BOOTSTRAP_FAMILY, LDest.StyleFamily, 'StyleFamily');
    Assert.AreEqual(btn_danger, LDest.StyleClass, 'StyleClass');
    Assert.AreEqual(BOOTSTRAP_OUTLINE, LDest.StyleAppearance, 'StyleAppearance');
    Assert.AreEqual('Source', LDest.Caption, 'Caption');
    Assert.AreEqual('Source hint', LDest.Hint, 'Hint');
  finally
    LDest.Free;
    LSource.Free;
  end;
end;

procedure TStyledButtonAssignTests.Assign_WindowedButton_CopiesStyleAndCaption;
var
  LSource, LDest: TStyledButton;
begin
  LSource := TStyledButton.Create(nil);
  LDest := TStyledButton.Create(nil);
  try
    LSource.StyleFamily := BOOTSTRAP_FAMILY;
    LSource.StyleClass := btn_success;
    LSource.Caption := 'Source';
    LDest.Assign(LSource);
    Assert.AreEqual(BOOTSTRAP_FAMILY, LDest.StyleFamily, 'StyleFamily');
    Assert.AreEqual(btn_success, LDest.StyleClass, 'StyleClass');
    Assert.AreEqual('Source', LDest.Caption, 'Caption');
  finally
    LDest.Free;
    LSource.Free;
  end;
end;

procedure TStyledButtonAssignTests.AssignTo_Action_StillWorks;
var
  LButton: TStyledButton;
  LAction: TAction;
begin
  LButton := TStyledButton.Create(nil);
  LAction := TAction.Create(nil);
  try
    LButton.Caption := 'From button';
    LAction.Assign(LButton);
    Assert.AreEqual('From button', LAction.Caption, 'TControl.AssignTo(TCustomAction) path');
  finally
    LAction.Free;
    LButton.Free;
  end;
end;

{ TStyledButtonStyleTests }

procedure TStyledButtonStyleTests.StyleFamilyChangedAlone_RecomputesTheAttributes;
var
  LButton: TStyledButton;
  N, P, S, H, D: TStyledButtonAttributes;
  LClass, LAppearance: string;
begin
  LButton := TStyledButton.Create(nil);
  TStyledTestUtils.NewAttributes(N, P, S, H, D);
  try
    // Classic/Windows by default; change the family only
    LButton.StyleFamily := BOOTSTRAP_FAMILY;
    // The expected attributes are those of the family default (Primary/Normal)
    LClass := LButton.StyleClass;
    LAppearance := LButton.StyleAppearance;
    Assert.IsTrue(StyleFamilyUpdateAttributes(BOOTSTRAP_FAMILY, LClass, LAppearance, N, P, S, H, D),
      'reference attributes for ' + LClass + '/' + LAppearance);
    Assert.AreEqual(ColorToRGB(N.ButtonColor), ColorToRGB(LButton.ButtonStyleNormal.ButtonColor),
      'Normal.ButtonColor must be the Bootstrap one, not the previous Classic colour');
    Assert.AreEqual(ColorToRGB(N.FontColor), ColorToRGB(LButton.ButtonStyleNormal.FontColor),
      'Normal.FontColor must be the Bootstrap one');
  finally
    TStyledTestUtils.FreeAttributes(N, P, S, H, D);
    LButton.Free;
  end;
end;

procedure TStyledButtonStyleTests.FamilyChangeToM3_ResetsUnknownClassAndAppearance;
var
  LButton: TStyledButton;
  N, P, S, H, D: TStyledButtonAttributes;
  LClass, LAppearance: string;
begin
  LButton := TStyledButton.Create(nil);
  TStyledTestUtils.NewAttributes(N, P, S, H, D);
  try
    LButton.StyleFamily := BOOTSTRAP_FAMILY;
    LButton.StyleClass := btn_danger;
    LButton.StyleAppearance := BOOTSTRAP_OUTLINE;
    Assert.AreEqual(btn_danger, LButton.StyleClass, 'precondition');

    LButton.StyleFamily := ANGULAR_M3_LIGHT_FAMILY;
    Assert.AreEqual(btn_Basic, LButton.StyleClass, 'class reset to the only M3 class');
    Assert.AreEqual(FilledAttr, LButton.StyleAppearance, 'appearance reset to the M3 default');
    LClass := LButton.StyleClass;
    LAppearance := LButton.StyleAppearance;
    Assert.IsTrue(StyleFamilyUpdateAttributes(ANGULAR_M3_LIGHT_FAMILY, LClass, LAppearance, N, P, S, H, D));
    Assert.AreEqual(ColorToRGB(N.ButtonColor), ColorToRGB(LButton.ButtonStyleNormal.ButtonColor),
      'Normal.ButtonColor must be the M3 Filled one, not the Bootstrap Danger one');
    Assert.AreEqual(ColorToRGB(N.FontColor), ColorToRGB(LButton.ButtonStyleNormal.FontColor));
    Assert.AreEqual(Ord(btRounded), Ord(LButton.StyleDrawType), 'M3 default shape');
  finally
    TStyledTestUtils.FreeAttributes(N, P, S, H, D);
    LButton.Free;
  end;
end;

procedure TStyledButtonStyleTests.FamilyChangeToM3_KeepsTheValidValues;
var
  LButton: TStyledButton;
begin
  LButton := TStyledButton.Create(nil);
  try
    LButton.StyleFamily := ANGULAR_LIGHT_FAMILY;
    LButton.StyleClass := btn_Basic;
    LButton.StyleAppearance := RaisedAttr;
    LButton.StyleFamily := ANGULAR_M3_LIGHT_FAMILY;
    Assert.AreEqual(btn_Basic, LButton.StyleClass, 'Basic exists in both families');
    Assert.AreEqual(FilledAttr, LButton.StyleAppearance, 'Raised does not exist in M3');

    LButton.StyleAppearance := OutlinedAttr;
    LButton.StyleFamily := ANGULAR_M3_DARK_FAMILY;
    Assert.AreEqual(OutlinedAttr, LButton.StyleAppearance, 'Outlined exists in M3 Dark');
    LButton.StyleFamily := ANGULAR_M3_LIGHT_FAMILY;
    Assert.AreEqual(OutlinedAttr, LButton.StyleAppearance, 'and back to Light');
  finally
    LButton.Free;
  end;
end;

procedure TStyledButtonStyleTests.FamilyChangeFromM3_ResetsToTheNewFamilyDefaults;
var
  LButton: TStyledButton;
  N, P, S, H, D: TStyledButtonAttributes;
  LClass, LAppearance: string;
begin
  LButton := TStyledButton.Create(nil);
  TStyledTestUtils.NewAttributes(N, P, S, H, D);
  try
    LButton.StyleFamily := ANGULAR_M3_LIGHT_FAMILY;
    LButton.StyleAppearance := TonalAttr;
    LButton.StyleFamily := BOOTSTRAP_FAMILY;
    LClass := LButton.StyleClass;
    LAppearance := LButton.StyleAppearance;
    Assert.AreNotEqual(TonalAttr, LAppearance, 'Tonal does not exist in Bootstrap');
    Assert.IsTrue(StyleFamilyUpdateAttributes(BOOTSTRAP_FAMILY, LClass, LAppearance, N, P, S, H, D));
    Assert.AreEqual(ColorToRGB(N.ButtonColor), ColorToRGB(LButton.ButtonStyleNormal.ButtonColor),
      'Normal.ButtonColor must be the Bootstrap one');
  finally
    TStyledTestUtils.FreeAttributes(N, P, S, H, D);
    LButton.Free;
  end;
end;

procedure TStyledButtonStyleTests.PanelFamilyChange_ResetsClassAndAppearance;
var
  LForm: TForm;
  LPanel: TStyledPanel;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LPanel := TStyledPanel.Create(LForm);
    LPanel.StyleFamily := BOOTSTRAP_FAMILY;
    LPanel.StyleClass := btn_danger;
    LPanel.StyleAppearance := BOOTSTRAP_OUTLINE;
    LPanel.StyleFamily := ANGULAR_M3_DARK_FAMILY;
    Assert.AreEqual(btn_Basic, LPanel.StyleClass, 'class reset');
    Assert.AreEqual(FilledAttr, LPanel.StyleAppearance, 'appearance reset');
    LPanel.StyleAppearance := TonalAttr;
    LPanel.StyleFamily := ANGULAR_M3_LIGHT_FAMILY;
    Assert.AreEqual(TonalAttr, LPanel.StyleAppearance, 'Tonal kept moving Dark -> Light');
  finally
    LForm.Free;
  end;
end;

{ TStyledButtonKeyboardTests }

procedure TStyledButtonKeyboardTests.CountClick(Sender: TObject);
begin
  Inc(FClicks);
end;

/// <summary>
///  Sends Esc (CN_KEYDOWN) to a Cancel button on a shown off-screen form with the thread's
///  modifier keys set as given, restoring the keyboard state after.  CMDialogKey reads the
///  modifiers with GetKeyState, so the test sets them instead of depending on the live state.
/// </summary>
function SendEscape(const AOnClick: TNotifyEvent; const AShiftDown: Boolean): Integer;
var
  LForm: TForm;
  LButton: TStyledButton;
  LSaved, LState: TKeyboardState;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    // CMDialogKey needs CanFocus, i.e. a visible form: show it off-screen
    LForm.Position := poDesigned;
    LForm.Left := -32000;
    LForm.Top := -32000;
    LButton := TStyledButton.Create(LForm);
    LButton.Parent := LForm;
    LButton.Cancel := True; // Esc is handled regardless of the Active/Default state
    LButton.OnClick := AOnClick;
    if not GetKeyboardState(LSaved) then
      RaiseLastOSError;
    LState := LSaved;
    LState[VK_SHIFT] := 0;
    LState[VK_LSHIFT] := 0;
    LState[VK_RSHIFT] := 0;
    LState[VK_CONTROL] := 0;
    LState[VK_LCONTROL] := 0;
    LState[VK_RCONTROL] := 0;
    LState[VK_MENU] := 0;
    LState[VK_LMENU] := 0;
    LState[VK_RMENU] := 0;
    if AShiftDown then
    begin
      LState[VK_SHIFT] := $80;
      LState[VK_LSHIFT] := $80;
    end;
    LForm.Show;
    try
      if not SetKeyboardState(LState) then
        RaiseLastOSError;
      try
        Result := LButton.Perform(CN_KEYDOWN, VK_ESCAPE, 0);
      finally
        SetKeyboardState(LSaved);
      end;
    finally
      LForm.Hide;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TStyledButtonKeyboardTests.CNKeyDown_DialogKeyHandled_ReturnsOneAndClicksOnce;
var
  LResult: Integer;
begin
  FClicks := 0;
  LResult := SendEscape(CountClick, False);
  Assert.AreEqual(1, FClicks, 'the dialog key must click the button exactly once');
  Assert.AreEqual(1, LResult, 'CN_KEYDOWN must report the handled dialog key (1)');
end;

procedure TStyledButtonKeyboardTests.CNKeyDown_WithShiftDown_DoesNotClick;
begin
  FClicks := 0;
  SendEscape(CountClick, True);
  Assert.AreEqual(0, FClicks, 'Shift+Esc must not click the Cancel button');
end;

initialization
  TDUnitX.RegisterTestFixture(TStyledButtonSmokeTests);
  TDUnitX.RegisterTestFixture(TStyledButtonAssignTests);
  TDUnitX.RegisterTestFixture(TStyledButtonStyleTests);
  TDUnitX.RegisterTestFixture(TStyledButtonKeyboardTests);

end.
