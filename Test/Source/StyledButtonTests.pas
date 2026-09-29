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
  Vcl.StyledButton,
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

{ TStyledButtonKeyboardTests }

procedure TStyledButtonKeyboardTests.CountClick(Sender: TObject);
begin
  Inc(FClicks);
end;

procedure TStyledButtonKeyboardTests.CNKeyDown_DialogKeyHandled_ReturnsOneAndClicksOnce;
var
  LForm: TForm;
  LButton: TStyledButton;
  LResult: Integer;
begin
  FClicks := 0;
  LForm := TStyledTestUtils.HostForm;
  try
    // CMDialogKey needs CanFocus, i.e. a visible form: show it off-screen
    LForm.Position := poDesigned;
    LForm.Left := -32000;
    LForm.Top := -32000;
    LButton := TStyledButton.Create(LForm);
    LButton.Parent := LForm;
    LButton.Cancel := True; // Esc is handled regardless of the Active/Default state
    LButton.OnClick := CountClick;
    LForm.Show;
    try
      LResult := LButton.Perform(CN_KEYDOWN, VK_ESCAPE, 0);
    finally
      LForm.Hide;
    end;
    Assert.AreEqual(1, FClicks, 'the dialog key must click the button exactly once');
    Assert.AreEqual(1, LResult, 'CN_KEYDOWN must report the handled dialog key (1)');
  finally
    LForm.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TStyledButtonSmokeTests);
  TDUnitX.RegisterTestFixture(TStyledButtonAssignTests);
  TDUnitX.RegisterTestFixture(TStyledButtonStyleTests);
  TDUnitX.RegisterTestFixture(TStyledButtonKeyboardTests);

end.
