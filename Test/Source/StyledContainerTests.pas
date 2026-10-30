/// <summary>
///  The container components: toolbar, navigators, button group, category buttons, panel.
/// </summary>
unit StyledContainerTests;

interface

uses
  System.SysUtils,
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  Vcl.ExtCtrls,
  Vcl.BootstrapButtonStyles,
  Vcl.ButtonGroup,
  Vcl.CategoryButtons,
  DUnitX.TestFramework,
  Vcl.ButtonStylesAttributes,
  Vcl.StandardButtonStyles,
  Vcl.AngularButtonStyles,
  Vcl.StyledToolbar,
  Vcl.DBCtrls,
  Vcl.StyledDbNavigator,
  Vcl.StyledButtonGroup,
  Vcl.StyledCategoryButtons,
  Vcl.StyledPanel,
  StyledTestUtils;

type
  [TestFixture]
  TStyledToolbarTests = class
  public
    /// <summary>Sanity: constructor defaults of ButtonWidth/ButtonHeight (23 x 22).</summary>
    [Test]
    procedure Create_Default_ButtonSize;

    /// <summary>
    ///  Regression (T2). EndUpdate only decremented the update counter and
    ///  ResizeButtons bails out while it is above zero, so ButtonWidth and
    ///  ButtonHeight set inside a BeginUpdate/EndUpdate block were never
    ///  applied to the buttons.
    /// </summary>
    [Test]
    procedure ButtonSize_SetInsideBeginUpdate_IsAppliedOnEndUpdate;

    /// <summary>
    ///  Regression (T3). A Wrapable AutoSize toolbar Align=alTop wrapped its buttons
    ///  on more rows but its height stayed at one row: ControlsHeight summed
    ///  heights instead of taking the bottom of the lowest control.
    /// </summary>
    [Test]
    procedure WrapableAutoSize_AlTop_HeightFollowsTheRows;

    /// <summary>Regression (T3), vertical toolbar Align=alLeft with FlowStyle fsTopBottomLeftRight.</summary>
    [Test]
    procedure WrapableAutoSize_AlLeft_WidthFollowsTheColumns;
  end;

  [TestFixture]
  TStyledPanelTests = class
  public
    /// <summary>
    ///  Regression (G3). SetStyleDrawType ran ParentBackground := not
    ///  IsStoredParentBackground also while loading, so a btRect panel saved
    ///  with ParentBackground = True came back opaque from the DFM.
    /// </summary>
    [Test]
    procedure StreamRoundTrip_RectWithParentBackground_KeepsParentBackground;

    /// <summary>
    ///  Regression (G1). With AsVCLComponent the panel checked the attributes
    ///  with the active VCL style name but built and painted them with
    ///  FStyleClass ('Windows'), so it never followed the active VCL style.
    ///  Loads Carbon.vsf from the Delphi Redist folder.
    /// </summary>
    [Test]
    procedure AsVCLComponent_PaintsWithTheActiveVclStyleColours;

    /// <summary>
    ///  Regression (G3 visible effect). A btRect Outline panel with
    ///  ParentBackground = True must show the parent's background inside its
    ///  border; today the interior is painted with Color (clBlack, see F1).
    /// </summary>
    [Test]
    [TestCase('PlainHost', 'False')]
    [TestCase('DoubleBufferedHost', 'True')]
    procedure OutlineRect_WithParentBackground_InteriorShowsTheParent(const ADoubleBuffered: Boolean);

    /// <summary>
    ///  Regression (P-G4). A Classic panel with an explicit StyleClass (a VCL
    ///  style name) took its colours from the button table: Coral was painted
    ///  orange like a Coral button instead of the light grey Coral panel
    ///  (RegisterPanelThemeAttributes).
    /// </summary>
    [Test]
    procedure Classic_ExplicitStyleClass_UsesThePanelColoursOfTheStyle;

    /// <summary>
    ///  Regression (P-G5). AsVCLComponent must look like a TPanel of the active
    ///  VCL style: square corners whatever StyleDrawType says.
    /// </summary>
    [Test]
    procedure AsVCLComponent_PaintsSquareCorners;

    /// <summary>
    ///  Component editor flow: the style chosen on a detached panel is copied to
    ///  the real panel with AssignStyleTo; the real panel must paint with it.
    /// </summary>
    [Test]
    procedure AssignStyleTo_FromBootstrapPanel_RepaintsTheTargetPanel;

    /// <summary>
    ///  Regression (P-G6, 10.4+). Setting a per-control StyleName on an
    ///  AsVCLComponent panel whose StyleClass already had that name did not
    ///  re-resolve the attributes: the panel kept the colours of the style
    ///  active when it was created (the global one) instead of its own.
    /// </summary>
    [Test]
    procedure StyleName_RepaintsWithThePanelColoursOfThatStyle;

    /// <summary>
    ///  Regression (P-G6b, 10.4+), the demo case: global style Carbon (dark),
    ///  AsVCLComponent panel with StyleName Windows10 (light) must paint the
    ///  Windows10 panel colour, not the global one.
    /// </summary>
    [Test]
    procedure StyleName_UnderAGlobalCustomStyle_PaintsItsOwnStyle;

    /// <summary>
    ///  Regression (G3c). Under a custom VCL style a host TPanel with seClient
    ///  is painted by its style hook, not with Color: the transparent styled
    ///  panel must show what the host really shows (as a TPanel with
    ///  ParentBackground does through StyleServices.DrawParentBackground),
    ///  not the host's Color obtained by sending it WM_ERASEBKGND.
    /// </summary>
    [Test]
    procedure OutlineRect_WithParentBackground_UnderVclStyle_ShowsTheStyledHost;
  end;

  [TestFixture]
  TStyleShadowSizeTests = class
  public
    /// <summary>The toolbar propagates StyleShadowSize to its buttons, like StyleRadius.</summary>
    [Test]
    procedure Toolbar_PropagatesToButtons_AndKeepsCustomButtons;
    /// <summary>The navigator propagates StyleShadowSize to its buttons.</summary>
    [Test]
    procedure DbNavigator_PropagatesToButtons;
    /// <summary>Group: the item inherits StyleShadowSize and the group paints the shadow inside each item.</summary>
    [Test]
    procedure ButtonGroup_ItemInheritsAndPaintsTheShadow;
    /// <summary>Category buttons: the item inherits StyleShadowSize and the control paints differently.</summary>
    [Test]
    procedure CategoryButtons_ItemInheritsAndPaintsTheShadow;
    /// <summary>Panel: default -1 (family), not stored; a value paints the shadow inside the control, is streamed and assigned.</summary>
    [Test]
    procedure Panel_StyleShadowSize_PaintsStreamsAndAssigns;
    /// <summary>Panel: Elevated has the shadow of the family, 0 removes it, values below -1 are refused.</summary>
    [Test]
    procedure Panel_Elevated_FamilyShadow_ZeroRemovesIt;
    /// <summary>Panel: the client area of the children excludes the shadow.</summary>
    [Test]
    procedure Panel_ChildrenStayInsideTheFace;
  end;

  [TestFixture]
  TStyleBorderWidthContainerTests = class
  public
    /// <summary>The toolbar propagates StyleBorderWidth to its buttons, like StyleRadius.</summary>
    [Test]
    procedure Toolbar_PropagatesToButtons_AndKeepsCustomButtons;
    /// <summary>The navigator propagates StyleBorderWidth to its buttons.</summary>
    [Test]
    procedure DbNavigator_PropagatesToButtons;
    /// <summary>Group: the item inherits StyleBorderWidth and the group paints it.</summary>
    [Test]
    procedure ButtonGroup_ItemInheritsAndPaintsTheBorder;
    /// <summary>Category buttons: the item inherits StyleBorderWidth and the control paints it.</summary>
    [Test]
    procedure CategoryButtons_ItemInheritsAndPaintsTheBorder;
    /// <summary>RegisterDefaultRenderingStyle gives the registered border width to the new containers.</summary>
    [Test]
    procedure RegisteredDefault_AppliesToNewContainers;
  end;

  [TestFixture]
  TStyledGroupItemTests = class
  public
    /// <summary>
    ///  Regression (G2). An item added to an EMPTY TStyledButtonGroup took the
    ///  class defaults instead of the container's DrawType/Radius, because
    ///  LoadDefaultStyles required a FStyleApplied that an empty group can
    ///  never set.
    /// </summary>
    [Test]
    procedure ButtonGroup_ItemAddedToEmptyGroup_InheritsContainerShape;

    /// <summary>Regression (G2). Same defect in TStyledCategoryButtons.</summary>
    [Test]
    procedure CategoryButtons_ItemAddedToEmptyCategory_InheritsContainerShape;
  end;

implementation

type
  //Access to the protected AdjustSize that the VCL calls after a resize
  TToolbarAccess = class(TStyledToolbar);

{ TStyledToolbarTests }

procedure TStyledToolbarTests.Create_Default_ButtonSize;
var
  LToolbar: TStyledToolbar;
begin
  LToolbar := TStyledToolbar.Create(nil);
  try
    Assert.AreEqual(23, LToolbar.ButtonWidth, 'ButtonWidth');
    Assert.AreEqual(22, LToolbar.ButtonHeight, 'ButtonHeight');
  finally
    LToolbar.Free;
  end;
end;

procedure TStyledToolbarTests.WrapableAutoSize_AlTop_HeightFollowsTheRows;
const
  BUTTONS = 6;
var
  LForm: TForm;
  LToolbar: TStyledToolbar;
  LButton: TStyledToolButton;
  I, LOneRow: Integer;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LForm.ClientWidth := 700;
    LForm.ClientHeight := 400;
    LToolbar := TStyledToolbar.Create(LForm);
    LToolbar.Parent := LForm;
    LToolbar.Align := alTop;
    LToolbar.ButtonWidth := 100;
    LToolbar.ButtonHeight := 30;
    for I := 1 to BUTTONS do
      Assert.IsTrue(LToolbar.NewButton(LButton), 'NewButton');
    LToolbar.Wrapable := True;
    LToolbar.AutoSize := True;
    LForm.HandleNeeded;
    LToolbar.HandleNeeded;
    TToolbarAccess(LToolbar).AdjustSize;
    LOneRow := LToolbar.Height;
    // narrow: 6 buttons of 100 do not fit in 250, so 3 rows of 2
    LForm.ClientWidth := 250;
    TToolbarAccess(LToolbar).AdjustSize;
    Assert.IsTrue(LToolbar.Height >= 3 * 30,
      Format('3 rows of 30 need at least 90, the toolbar is %d high (one row was %d)',
      [LToolbar.Height, LOneRow]));
    // wide again: back to one row
    LForm.ClientWidth := 700;
    TToolbarAccess(LToolbar).AdjustSize;
    Assert.IsTrue(LToolbar.Height <= LOneRow + 2,
      Format('one row again: %d, expected about %d', [LToolbar.Height, LOneRow]));
  finally
    LForm.Free;
  end;
end;

procedure TStyledToolbarTests.WrapableAutoSize_AlLeft_WidthFollowsTheColumns;
const
  BUTTONS = 6;
var
  LForm: TForm;
  LToolbar: TStyledToolbar;
  LButton: TStyledToolButton;
  I: Integer;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LForm.ClientWidth := 600;
    LForm.ClientHeight := 400;
    LToolbar := TStyledToolbar.Create(LForm);
    LToolbar.Parent := LForm;
    LToolbar.Align := alLeft;
    LToolbar.FlowStyle := fsTopBottomLeftRight;
    LToolbar.ButtonWidth := 100;
    LToolbar.ButtonHeight := 30;
    for I := 1 to BUTTONS do
      Assert.IsTrue(LToolbar.NewButton(LButton), 'NewButton');
    LToolbar.Wrapable := True;
    LToolbar.AutoSize := True;
    LForm.HandleNeeded;
    LToolbar.HandleNeeded;
    // short parent: 6 buttons of 30 do not fit in 100, so 2 columns
    LForm.ClientHeight := 100;
    TToolbarAccess(LToolbar).AdjustSize;
    Assert.IsTrue(LToolbar.Width >= 2 * 100,
      Format('2 columns of 100 need at least 200, the toolbar is %d wide', [LToolbar.Width]));
  finally
    LForm.Free;
  end;
end;

procedure TStyledToolbarTests.ButtonSize_SetInsideBeginUpdate_IsAppliedOnEndUpdate;
var
  LForm: TForm;
  LToolbar: TStyledToolbar;
  LButton: TStyledToolButton;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LToolbar := TStyledToolbar.Create(LForm);
    LToolbar.Parent := LForm;
    Assert.IsTrue(LToolbar.NewButton(LButton), 'NewButton');
    LToolbar.BeginUpdate;
    try
      LToolbar.ButtonWidth := 60;
      LToolbar.ButtonHeight := 40;
    finally
      LToolbar.EndUpdate;
    end;
    Assert.AreEqual(60, LToolbar.Buttons[0].Width, 'button Width after EndUpdate');
    Assert.AreEqual(40, LToolbar.Buttons[0].Height, 'button Height after EndUpdate');
  finally
    LForm.Free;
  end;
end;

{ TStyledPanelTests }

procedure TStyledPanelTests.StreamRoundTrip_RectWithParentBackground_KeepsParentBackground;
var
  LForm: TForm;
  LPanel, LCopy: TStyledPanel;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LPanel := TStyledPanel.Create(LForm);
    LPanel.StyleDrawType := btRect;
    LPanel.ParentBackground := True;
    LCopy := TStyledTestUtils.StreamRoundTrip(LPanel, TStyledPanel, LForm) as TStyledPanel;
    Assert.IsTrue(LCopy.StyleDrawType = btRect, 'StyleDrawType survives streaming');
    Assert.IsTrue(LCopy.ParentBackground, 'ParentBackground = True must survive streaming');
    //And it must also survive parenting, handle creation and the first paint
    LCopy.Parent := LForm;
    TStyledTestUtils.PaintWinControl(LCopy).Free;
    Assert.IsTrue(LCopy.ParentBackground, 'ParentBackground = True must survive parenting and painting');
  finally
    LForm.Free;
  end;
end;

procedure TStyledPanelTests.AsVCLComponent_PaintsWithTheActiveVclStyleColours;
const
  STYLE_NAME = 'Carbon';
var
  LStyleFile: string;
  LForm: TForm;
  LPanel: TStyledPanel;
  LBitmap: TBitmap;
  LTheme: TPanelThemeAttribute;
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
    Assert.IsTrue(GetPanelStyleAttributes(STYLE_NAME, LTheme), STYLE_NAME + ' panel colours are registered');
    LForm := TStyledTestUtils.HostForm;
    try
      LPanel := TStyledPanel.CreateStyled(LForm, DEFAULT_CLASSIC_FAMILY,
        DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE);
      LPanel.Parent := LForm;
      LPanel.SetBounds(0, 0, 200, 80);
      Assert.IsTrue(LPanel.AsVCLComponent, 'AsVCLComponent');
      LBitmap := TStyledTestUtils.PaintWinControl(LPanel);
      try
        TStyledTestUtils.AssertPixel(LBitmap, 100, 40, ColorToRGB(LTheme.PanelColor),
          STYLE_NAME + ' panel colour at the centre');
      finally
        LBitmap.Free;
      end;
    finally
      LForm.Free;
    end;
  finally
    TStyleManager.SetStyle(TStyleManager.SystemStyle);
  end;
end;

procedure TStyledPanelTests.OutlineRect_WithParentBackground_UnderVclStyle_ShowsTheStyledHost;
const
  STYLE_NAME = 'Carbon';
var
  LStyleFile: string;
  LForm: TForm;
  LHost, LReference: TPanel;
  LPanel: TStyledPanel;
  LStyled, LVcl: TBitmap;
  LHostColor: TColor;
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
    LForm := TStyledTestUtils.HostForm;
    try
      // Like the demo form: DoubleBuffered propagates to the host through
      // ParentDoubleBuffered and changes the erase path TWinControl takes
      LForm.DoubleBuffered := True;
      // Host: Color is sky blue but seClient lets the style paint it (dark grey)
      LHost := TPanel.Create(LForm);
      LHost.Parent := LForm;
      LHost.SetBounds(0, 0, 300, 220);
      LHost.BevelOuter := bvNone;
      LHost.Caption := '';
      LHost.ParentBackground := False;
      LHost.Color := clSkyBlue;
      // Reference: what the VCL does for a transparent TPanel on that host
      LReference := TPanel.Create(LForm);
      LReference.Parent := LHost;
      LReference.SetBounds(10, 110, 200, 80);
      LReference.BevelOuter := bvNone;
      LReference.Caption := '';
      LReference.ParentBackground := True;
      LPanel := TStyledPanel.CreateStyled(LForm, BOOTSTRAP_FAMILY, btn_primary, BOOTSTRAP_OUTLINE);
      LPanel.Parent := LHost;
      LPanel.SetBounds(10, 10, 200, 80);
      LPanel.StyleDrawType := btRect;
      LPanel.ParentBackground := True;
      LVcl := TStyledTestUtils.PaintWinControl(LReference);
      LStyled := TStyledTestUtils.PaintWinControl(LPanel);
      try
        LHostColor := TStyledTestUtils.PixelAt(LVcl, 100, 40);
        Assert.IsFalse(TStyledTestUtils.SameColor(LHostColor, ColorToRGB(clSkyBlue), 8),
          'precondition: under ' + STYLE_NAME + ' the VCL reference panel is not sky blue but ' +
          TStyledTestUtils.ColorText(LHostColor));
        TStyledTestUtils.AssertPixel(LStyled, 100, 40, LHostColor,
          'styled transparent panel interior must match the VCL transparent panel on the same host');
      finally
        LStyled.Free;
        LVcl.Free;
      end;
    finally
      LForm.Free;
    end;
  finally
    TStyleManager.SetStyle(TStyleManager.SystemStyle);
  end;
end;

procedure TStyledPanelTests.Classic_ExplicitStyleClass_UsesThePanelColoursOfTheStyle;
const
  STYLE_NAME = 'Coral';
var
  LForm: TForm;
  LPanel: TStyledPanel;
  LBitmap: TBitmap;
  LPanelTheme: TPanelThemeAttribute;
  LButtonTheme: TThemeAttribute;
begin
  Assert.IsTrue(GetPanelStyleAttributes(STYLE_NAME, LPanelTheme), STYLE_NAME + ' panel colours registered');
  Assert.IsTrue(GetStyleAttributes(STYLE_NAME, LButtonTheme), STYLE_NAME + ' button colours registered');
  Assert.AreNotEqual(ColorToRGB(LPanelTheme.PanelColor), ColorToRGB(LButtonTheme.ButtonColor),
    'precondition: the Coral panel and button colours differ');
  LForm := TStyledTestUtils.HostForm;
  try
    LPanel := TStyledPanel.CreateStyled(LForm, DEFAULT_CLASSIC_FAMILY, STYLE_NAME, DEFAULT_APPEARANCE);
    LPanel.Parent := LForm;
    LPanel.SetBounds(0, 0, 200, 80);
    LPanel.StyleDrawType := btRect;
    LPanel.ParentBackground := False;
    Assert.IsFalse(LPanel.AsVCLComponent, 'precondition: an explicit class is not AsVCLComponent');
    // the style colours are not user customisations: they must not be streamed in the DFM
    Assert.IsFalse(LPanel.PanelStyleNormal.HasCustomButtonColor, 'ButtonColor must not be flagged as custom');
    LBitmap := TStyledTestUtils.PaintWinControl(LPanel);
    try
      TStyledTestUtils.AssertPixel(LBitmap, 100, 40, ColorToRGB(LPanelTheme.PanelColor),
        STYLE_NAME + ' panel colour at the centre (not the button colour ' +
        TStyledTestUtils.ColorText(ColorToRGB(LButtonTheme.ButtonColor)) + ')');
    finally
      LBitmap.Free;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TStyledPanelTests.StyleName_RepaintsWithThePanelColoursOfThatStyle;
const
  STYLE_NAME = 'Carbon';
var
  LStyleFile: string;
  LForm: TForm;
  LPanel: TStyledPanel;
  LBitmap: TBitmap;
  LTheme, LWindows: TPanelThemeAttribute;
begin
  {$IF CompilerVersion < 34} // the library's D10_4+ symbol is not visible here
  Assert.Pass('per-control StyleName needs Delphi 10.4+');
  {$IFEND}
  LStyleFile := IncludeTrailingPathDelimiter(GetEnvironmentVariable('BDS')) +
    'Redist\styles\vcl\' + STYLE_NAME + '.vsf';
  if not FileExists(LStyleFile) then
    LStyleFile := 'C:\BDS\Studio\37.0\Redist\styles\vcl\' + STYLE_NAME + '.vsf';
  if not FileExists(LStyleFile) then
    Assert.Pass(STYLE_NAME + '.vsf not found: VCL style test skipped');
  if TStyleManager.Style[STYLE_NAME] = nil then
    TStyleManager.LoadFromFile(LStyleFile);
  // the global style stays the system one: only the panel gets Carbon
  Assert.IsTrue(GetPanelStyleAttributes(STYLE_NAME, LTheme), STYLE_NAME + ' panel colours registered');
  Assert.IsTrue(GetPanelStyleAttributes(DEFAULT_WINDOWS_CLASS, LWindows), 'Windows panel colours registered');
  LForm := TStyledTestUtils.HostForm;
  try
    // Like the demo: created with the style name as class, then AsVCLComponent,
    // then the per-control StyleName (the class name does not change)
    LPanel := TStyledPanel.CreateStyled(LForm, DEFAULT_CLASSIC_FAMILY, STYLE_NAME, DEFAULT_APPEARANCE);
    LPanel.SetBounds(0, 0, 200, 80);
    LPanel.ParentBackground := False;
    LPanel.AsVCLComponent := True;
    LPanel.StyleName := STYLE_NAME;
    LPanel.Parent := LForm; // like the demo: the parent comes last
    LBitmap := TStyledTestUtils.PaintWinControl(LPanel);
    try
      TStyledTestUtils.AssertPixel(LBitmap, 100, 40, ColorToRGB(LTheme.PanelColor),
        STYLE_NAME + ' panel colour at the centre (Windows would be ' +
        TStyledTestUtils.ColorText(ColorToRGB(LWindows.PanelColor)) + ')');
    finally
      LBitmap.Free;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TStyledPanelTests.StyleName_UnderAGlobalCustomStyle_PaintsItsOwnStyle;
const
  GLOBAL_STYLE = 'Carbon';
  PANEL_STYLE = 'Windows10';
  function LoadStyle(const AName: string): Boolean;
  var
    LFile: string;
  begin
    LFile := IncludeTrailingPathDelimiter(GetEnvironmentVariable('BDS')) + 'Redist\styles\vcl\' + AName + '.vsf';
    if not FileExists(LFile) then
      LFile := 'C:\BDS\Studio\37.0\Redist\styles\vcl\' + AName + '.vsf';
    Result := FileExists(LFile);
    if Result and (TStyleManager.Style[AName] = nil) then
      TStyleManager.LoadFromFile(LFile);
  end;
var
  LForm: TForm;
  LScrollBox: TScrollBox;
  LPanel: TStyledPanel;
  LBitmap: TBitmap;
  LTheme, LGlobal: TPanelThemeAttribute;
begin
  {$IF CompilerVersion < 34} // the library's D10_4+ symbol is not visible here
  Assert.Pass('per-control StyleName needs Delphi 10.4+');
  {$IFEND}
  if not (LoadStyle(GLOBAL_STYLE) and LoadStyle(PANEL_STYLE)) then
    Assert.Pass('style files not found: VCL style test skipped');
  Assert.IsTrue(GetPanelStyleAttributes(PANEL_STYLE, LTheme), PANEL_STYLE + ' panel colours registered');
  Assert.IsTrue(GetPanelStyleAttributes(GLOBAL_STYLE, LGlobal), GLOBAL_STYLE + ' panel colours registered');
  Assert.IsTrue(TStyleManager.TrySetStyle(GLOBAL_STYLE, False), 'TrySetStyle ' + GLOBAL_STYLE);
  try
    LForm := TStyledTestUtils.HostForm;
    try
      // Exactly the demo sequence
      LPanel := TStyledPanel.CreateStyled(LForm, DEFAULT_CLASSIC_FAMILY, PANEL_STYLE, DEFAULT_APPEARANCE);
      LPanel.SetBounds(0, 0, 200, 80);
      LPanel.ParentBackground := False;
      LPanel.AsVCLComponent := True;
      LPanel.StyleName := PANEL_STYLE;
      // like the demo: the parent (a hidden scroll box, shown afterwards) comes last
      LScrollBox := TScrollBox.Create(LForm);
      LScrollBox.Parent := LForm;
      LScrollBox.Align := alClient;
      LScrollBox.Visible := False;
      LPanel.Parent := LScrollBox;
      LScrollBox.Visible := True;
      LForm.Position := poDesigned;
      LForm.Left := -32000;
      LForm.Top := -32000;
      LForm.Show;
      try
        LPanel.Repaint;
        LBitmap := TStyledTestUtils.PaintWinControl(LPanel);
      finally
        LForm.Hide;
      end;
      try
        TStyledTestUtils.AssertPixel(LBitmap, 100, 40, ColorToRGB(LTheme.PanelColor),
          Format('%s panel colour at the centre (the global %s would be %s) - AsVCL=%s seClient=%s ' +
            'Normal.ButtonColor=%s Color=%s ActiveStyleName=%s',
            [PANEL_STYLE, GLOBAL_STYLE, TStyledTestUtils.ColorText(ColorToRGB(LGlobal.PanelColor)),
             BoolToStr(LPanel.AsVCLComponent, True), BoolToStr(seClient in LPanel.StyleElements, True),
             TStyledTestUtils.ColorText(ColorToRGB(LPanel.PanelStyleNormal.ButtonColor)),
             TStyledTestUtils.ColorText(ColorToRGB(LPanel.Color)), LPanel.ActiveStyleName]));
      finally
        LBitmap.Free;
      end;
    finally
      LForm.Free;
    end;
  finally
    TStyleManager.SetStyle(TStyleManager.SystemStyle);
  end;
end;

procedure TStyledPanelTests.AssignStyleTo_FromBootstrapPanel_RepaintsTheTargetPanel;
var
  LForm: TForm;
  LReal, LDest, LRef: TStyledPanel;
  LBitmap: TBitmap;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    // the panel in the designer: default Classic/Windows, not transparent
    LReal := TStyledPanel.Create(LForm);
    LReal.Parent := LForm;
    LReal.SetBounds(0, 0, 200, 80);
    LReal.ParentBackground := False;
    // reference: a panel created directly as Bootstrap/Success
    LRef := TStyledPanel.CreateStyled(LForm, BOOTSTRAP_FAMILY, 'Success', BOOTSTRAP_NORMAL);
    LRef.Parent := LForm;
    LRef.SetBounds(0, 100, 200, 80);
    LRef.ParentBackground := False;
    // the editor's "new" panel receives the chosen preview style
    LDest := TStyledPanel.CreateStyled(LForm, DEFAULT_CLASSIC_FAMILY, DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE);
    LDest.StyleFamily := BOOTSTRAP_FAMILY;
    LDest.StyleClass := 'Success';
    LDest.StyleAppearance := BOOTSTRAP_NORMAL;
    LDest.AssignStyleTo(LReal);
    LBitmap := TStyledTestUtils.PaintWinControl(LReal);
    try
      Assert.AreEqual('Success', LReal.StyleClass, 'StyleClass');
      Assert.AreEqual(BOOTSTRAP_FAMILY, LReal.StyleFamily, 'StyleFamily');
      TStyledTestUtils.AssertPixel(LBitmap, 100, 40, ColorToRGB(LRef.PanelStyleNormal.ButtonColor),
        Format('target panel after AssignStyleTo (AsVCL=%s ParentBackground=%s Color=%s Normal=%s)',
        [BoolToStr(LReal.AsVCLComponent, True), BoolToStr(LReal.ParentBackground, True),
         TStyledTestUtils.ColorText(ColorToRGB(LReal.Color)),
         TStyledTestUtils.ColorText(ColorToRGB(LReal.PanelStyleNormal.ButtonColor))]));
    finally
      LBitmap.Free;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TStyledPanelTests.AsVCLComponent_PaintsSquareCorners;
var
  LForm: TForm;
  LHost: TPanel;
  LPanel: TStyledPanel;
  LBitmap: TBitmap;
  LTheme: TPanelThemeAttribute;
begin
  // system style: the active style name resolves to 'Windows'
  Assert.IsTrue(GetPanelStyleAttributes(DEFAULT_WINDOWS_CLASS, LTheme), 'Windows panel colours registered');
  LForm := TStyledTestUtils.HostForm;
  try
    // Sky-blue host + ParentBackground: whatever the shape leaves uncovered
    // shows the host, so a rounded corner is detectable at (1,1)
    LHost := TPanel.Create(LForm);
    LHost.Parent := LForm;
    LHost.SetBounds(0, 0, 300, 120);
    LHost.BevelOuter := bvNone;
    LHost.Caption := '';
    LHost.ParentBackground := False;
    LHost.Color := clSkyBlue;
    LPanel := TStyledPanel.CreateStyled(LForm, DEFAULT_CLASSIC_FAMILY, DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE);
    LPanel.Parent := LHost;
    LPanel.SetBounds(10, 10, 200, 80);
    LPanel.StyleDrawType := btRounded; // must be ignored by AsVCLComponent
    LPanel.AsVCLComponent := True;
    // opaque: a rounded shape (StyleDrawType sets ParentBackground) would be
    // transparent, and an opaque one erases its corners with Color anyway, so
    // the shape is read from the border: a square panel has its border at (0,0)
    LPanel.ParentBackground := False;
    Assert.IsTrue(LPanel.AsVCLComponent, 'precondition: AsVCLComponent');
    Assert.AreNotEqual(ColorToRGB(LTheme.BorderColor), ColorToRGB(LTheme.PanelColor),
      'precondition: Windows panel border and panel colours differ');
    LBitmap := TStyledTestUtils.PaintWinControl(LPanel);
    try
      TStyledTestUtils.AssertPixel(LBitmap, 0, 0, ColorToRGB(LTheme.BorderColor),
        'AsVCLComponent panel must draw its border up to the corner like a square TPanel');
      TStyledTestUtils.AssertPixel(LBitmap, 0, 40, ColorToRGB(LTheme.BorderColor),
        'left border at mid height');
      TStyledTestUtils.AssertPixel(LBitmap, 100, 40, ColorToRGB(LTheme.PanelColor),
        'Windows panel colour at the centre');
    finally
      LBitmap.Free;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TStyledPanelTests.OutlineRect_WithParentBackground_InteriorShowsTheParent(
  const ADoubleBuffered: Boolean);
var
  LForm: TForm;
  LHost: TPanel;
  LPanel: TStyledPanel;
  LBitmap: TBitmap;
  LWhat: string;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LForm.DoubleBuffered := ADoubleBuffered;
    LHost := TPanel.Create(LForm);
    LHost.Parent := LForm;
    LHost.SetBounds(0, 0, 300, 120);
    LHost.BevelOuter := bvNone;
    LHost.Caption := '';
    LHost.ParentBackground := False;
    LHost.Color := clSkyBlue;
    LPanel := TStyledPanel.CreateStyled(LForm, BOOTSTRAP_FAMILY, btn_primary, BOOTSTRAP_OUTLINE);
    LPanel.Parent := LHost;
    LPanel.SetBounds(10, 10, 200, 80);
    LPanel.StyleDrawType := btRect;
    LPanel.ParentBackground := True;
    LBitmap := TStyledTestUtils.PaintWinControl(LPanel);
    try
      LWhat := Format('interior of an Outline btRect panel with ParentBackground ' +
        '(Color=%s ButtonColor=%s ButtonDrawStyle=%d BrushStyle=%d PB=%s)',
        [TStyledTestUtils.ColorText(ColorToRGB(LPanel.Color)),
         TStyledTestUtils.ColorText(ColorToRGB(LPanel.PanelStyleNormal.ButtonColor)),
         Ord(LPanel.PanelStyleNormal.ButtonDrawStyle),
         Ord(LPanel.PanelStyleNormal.BrushStyle),
         BoolToStr(LPanel.ParentBackground, True)]);
      TStyledTestUtils.AssertPixel(LBitmap, 100, 40, ColorToRGB(clSkyBlue), LWhat);
    finally
      LBitmap.Free;
    end;
  finally
    LForm.Free;
  end;
end;

//True when at least one pixel is different
function BitmapsDiffer(const A, B: TBitmap): Boolean;
var
  X, Y: Integer;
begin
  Result := False;
  for Y := 0 to A.Height - 1 do
    for X := 0 to A.Width - 1 do
      if TStyledTestUtils.PixelAt(A, X, Y) <> TStyledTestUtils.PixelAt(B, X, Y) then
        Exit(True);
end;

{ TStyleBorderWidthContainerTests }

procedure TStyleBorderWidthContainerTests.RegisteredDefault_AppliesToNewContainers;
var
  LForm: TForm;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    TStyledToolbar.RegisterDefaultRenderingStyle(DEFAULT_STYLEDRAWTYPE, DEFAULT_CLASSIC_FAMILY,
      DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE, DEFAULT_RADIUS, 3);
    TStyledDbNavigator.RegisterDefaultRenderingStyle(DEFAULT_STYLEDRAWTYPE, DEFAULT_CLASSIC_FAMILY,
      DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE, DEFAULT_RADIUS, 3, DEFAULT_CURSOR);
    TStyledButtonGroup.RegisterDefaultRenderingStyle(DEFAULT_STYLEDRAWTYPE, DEFAULT_CLASSIC_FAMILY,
      DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE, DEFAULT_RADIUS, 3, DEFAULT_CURSOR);
    TStyledCategoryButtons.RegisterDefaultRenderingStyle(DEFAULT_STYLEDRAWTYPE, DEFAULT_CLASSIC_FAMILY,
      DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE, DEFAULT_RADIUS, 3, DEFAULT_CURSOR);
    try
      Assert.AreEqual(3, TStyledToolbar.Create(LForm).StyleBorderWidth, 'toolbar');
      Assert.AreEqual(3, TStyledDbNavigator.Create(LForm).StyleBorderWidth, 'navigator');
      Assert.AreEqual(3, TStyledButtonGroup.Create(LForm).StyleBorderWidth, 'group');
      Assert.AreEqual(3, TStyledCategoryButtons.Create(LForm).StyleBorderWidth, 'category');
    finally
      TStyledToolbar.RegisterDefaultRenderingStyle(DEFAULT_STYLEDRAWTYPE, DEFAULT_CLASSIC_FAMILY,
        DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE, DEFAULT_RADIUS, DEFAULT_BORDER_WIDTH);
      TStyledDbNavigator.RegisterDefaultRenderingStyle(DEFAULT_STYLEDRAWTYPE, DEFAULT_CLASSIC_FAMILY,
        DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE, DEFAULT_RADIUS, DEFAULT_BORDER_WIDTH, DEFAULT_CURSOR);
      TStyledButtonGroup.RegisterDefaultRenderingStyle(DEFAULT_STYLEDRAWTYPE, DEFAULT_CLASSIC_FAMILY,
        DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE, DEFAULT_RADIUS, DEFAULT_BORDER_WIDTH, DEFAULT_CURSOR);
      TStyledCategoryButtons.RegisterDefaultRenderingStyle(DEFAULT_STYLEDRAWTYPE, DEFAULT_CLASSIC_FAMILY,
        DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE, DEFAULT_RADIUS, DEFAULT_BORDER_WIDTH, DEFAULT_CURSOR);
    end;
  finally
    LForm.Free;
  end;
end;

procedure TStyleBorderWidthContainerTests.Toolbar_PropagatesToButtons_AndKeepsCustomButtons;
var
  LForm: TForm;
  LToolbar: TStyledToolbar;
  LButton1, LButton2: TStyledToolButton;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LToolbar := TStyledToolbar.Create(LForm);
    LToolbar.Parent := LForm;
    Assert.IsTrue(LToolbar.NewButton(LButton1));
    Assert.IsTrue(LToolbar.NewButton(LButton2));
    Assert.AreEqual(DEFAULT_BORDER_WIDTH, LToolbar.StyleBorderWidth, 'default');
    LButton2.StyleBorderWidth := 6;
    LToolbar.StyleBorderWidth := 3;
    Assert.AreEqual(3, LButton1.StyleBorderWidth, 'button follows the toolbar');
    Assert.AreEqual(6, LButton2.StyleBorderWidth, 'a button with its own value keeps it');
    //a button added later inherits the toolbar value
    Assert.IsTrue(LToolbar.NewButton(LButton1));
    Assert.AreEqual(3, LButton1.StyleBorderWidth, 'new button inherits');
    Assert.IsTrue(Pos('StyleBorderWidth = 3', TStyledTestUtils.DfmText(LToolbar)) > 0, 'stored');
    LToolbar.StyleBorderWidth := DEFAULT_BORDER_WIDTH;
    Assert.AreEqual(DEFAULT_BORDER_WIDTH, LToolbar.StyleBorderWidth);
  finally
    LForm.Free;
  end;
end;

procedure TStyleBorderWidthContainerTests.DbNavigator_PropagatesToButtons;
var
  LForm: TForm;
  LNavigator: TStyledDBNavigator;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LNavigator := TStyledDBNavigator.Create(LForm);
    LNavigator.Parent := LForm;
    Assert.AreEqual(DEFAULT_BORDER_WIDTH, LNavigator.StyleBorderWidth, 'default');
    LNavigator.StyleBorderWidth := 3;
    Assert.AreEqual(3, LNavigator.Buttons[nbFirst].StyleBorderWidth, 'button follows the navigator');
    Assert.AreEqual(3, LNavigator.Buttons[nbDelete].StyleBorderWidth, 'all the buttons');
    Assert.IsTrue(Pos('StyleBorderWidth = 3', TStyledTestUtils.DfmText(LNavigator)) > 0, 'stored');
  finally
    LForm.Free;
  end;
end;

procedure TStyleBorderWidthContainerTests.ButtonGroup_ItemInheritsAndPaintsTheBorder;
var
  LForm: TForm;
  LGroup: TStyledButtonGroup;
  LItem: TStyledGrpButtonItem;
  LPlain, LThick: TBitmap;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LGroup := TStyledButtonGroup.Create(LForm);
    LGroup.Parent := LForm;
    LGroup.SetBounds(0, 0, 200, 80);
    LGroup.StyleFamily := BOOTSTRAP_FAMILY;
    LGroup.StyleClass := btn_primary;
    LGroup.StyleAppearance := BOOTSTRAP_OUTLINE;
    LItem := LGroup.Items.Add as TStyledGrpButtonItem;
    LItem.Caption := 'A';
    Assert.AreEqual(DEFAULT_BORDER_WIDTH, LItem.StyleBorderWidth, 'default');
    LPlain := TStyledTestUtils.PaintWinControl(LGroup);
    try
      LGroup.StyleBorderWidth := 6;
      Assert.AreEqual(6, LItem.StyleBorderWidth, 'item inherits the group value');
      LThick := TStyledTestUtils.PaintWinControl(LGroup);
      try
        Assert.IsTrue(BitmapsDiffer(LPlain, LThick), 'the item is painted with a thicker border');
      finally
        LThick.Free;
      end;
    finally
      LPlain.Free;
    end;
    Assert.IsTrue(Pos('StyleBorderWidth = 6', TStyledTestUtils.DfmText(LGroup)) > 0, 'stored');
    LItem.StyleBorderWidth := 0;
    Assert.AreEqual(0, LItem.StyleBorderWidth, 'an item can have its own value');
  finally
    LForm.Free;
  end;
end;

procedure TStyleBorderWidthContainerTests.CategoryButtons_ItemInheritsAndPaintsTheBorder;
var
  LForm: TForm;
  LCategoryButtons: TStyledCategoryButtons;
  LCategory: TButtonCategory;
  LItem: TStyledButtonItem;
  LPlain, LThick: TBitmap;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LCategoryButtons := TStyledCategoryButtons.Create(LForm);
    LCategoryButtons.Parent := LForm;
    LCategoryButtons.SetBounds(0, 0, 200, 120);
    LCategoryButtons.StyleFamily := BOOTSTRAP_FAMILY;
    LCategoryButtons.StyleClass := btn_primary;
    LCategoryButtons.StyleAppearance := BOOTSTRAP_OUTLINE;
    LCategory := LCategoryButtons.Categories.Add;
    LCategory.Caption := 'Cat';
    LItem := LCategory.Items.Add as TStyledButtonItem;
    LItem.Caption := 'A';
    Assert.AreEqual(DEFAULT_BORDER_WIDTH, LItem.StyleBorderWidth, 'default');
    LPlain := TStyledTestUtils.PaintWinControl(LCategoryButtons);
    try
      LCategoryButtons.StyleBorderWidth := 6;
      Assert.AreEqual(6, LItem.StyleBorderWidth, 'item inherits the container value');
      LThick := TStyledTestUtils.PaintWinControl(LCategoryButtons);
      try
        Assert.IsTrue(BitmapsDiffer(LPlain, LThick), 'the item is painted with a thicker border');
      finally
        LThick.Free;
      end;
    finally
      LPlain.Free;
    end;
    Assert.IsTrue(Pos('StyleBorderWidth = 6', TStyledTestUtils.DfmText(LCategoryButtons)) > 0, 'stored');
  finally
    LForm.Free;
  end;
end;

{ TStyleShadowSizeTests }

//Paints a panel like a WM_ERASEBKGND + WM_PAINT: PaintTo of a panel with
//ParentBackground does not reach Paint
function PaintPanelFully(const APanel: TStyledPanel): TBitmap;
begin
  APanel.HandleNeeded;
  Result := TStyledTestUtils.NewBitmap(APanel.Width, APanel.Height, clPureWhite);
  APanel.Perform(WM_ERASEBKGND, WPARAM(Result.Canvas.Handle), 0);
  APanel.Perform(WM_PAINT, WPARAM(Result.Canvas.Handle), 0);
end;

procedure TStyleShadowSizeTests.Toolbar_PropagatesToButtons_AndKeepsCustomButtons;
var
  LForm: TForm;
  LToolbar: TStyledToolbar;
  LButton1, LButton2: TStyledToolButton;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LToolbar := TStyledToolbar.Create(LForm);
    LToolbar.Parent := LForm;
    Assert.IsTrue(LToolbar.NewButton(LButton1));
    Assert.IsTrue(LToolbar.NewButton(LButton2));
    Assert.AreEqual(DEFAULT_SHADOW_SIZE, LToolbar.StyleShadowSize, 'default');
    LButton2.StyleShadowSize := 6;
    LToolbar.StyleShadowSize := 3;
    Assert.AreEqual(3, LButton1.StyleShadowSize, 'button follows the toolbar');
    Assert.AreEqual(6, LButton2.StyleShadowSize, 'a button with its own value keeps it');
    //a button added later inherits the toolbar value
    Assert.IsTrue(LToolbar.NewButton(LButton1));
    Assert.AreEqual(3, LButton1.StyleShadowSize, 'new button inherits');
    Assert.IsTrue(Pos('StyleShadowSize = 3', TStyledTestUtils.DfmText(LToolbar)) > 0, 'stored');
    LToolbar.StyleShadowSize := DEFAULT_SHADOW_SIZE;
    Assert.AreEqual(DEFAULT_SHADOW_SIZE, LToolbar.StyleShadowSize);
  finally
    LForm.Free;
  end;
end;

procedure TStyleShadowSizeTests.DbNavigator_PropagatesToButtons;
var
  LForm: TForm;
  LNavigator: TStyledDBNavigator;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LNavigator := TStyledDBNavigator.Create(LForm);
    LNavigator.Parent := LForm;
    Assert.AreEqual(DEFAULT_SHADOW_SIZE, LNavigator.StyleShadowSize, 'default');
    LNavigator.StyleShadowSize := 3;
    Assert.AreEqual(3, LNavigator.Buttons[nbFirst].StyleShadowSize, 'button follows the navigator');
    Assert.AreEqual(3, LNavigator.Buttons[nbDelete].StyleShadowSize, 'all the buttons');
    Assert.IsTrue(Pos('StyleShadowSize = 3', TStyledTestUtils.DfmText(LNavigator)) > 0, 'stored');
  finally
    LForm.Free;
  end;
end;

procedure TStyleShadowSizeTests.ButtonGroup_ItemInheritsAndPaintsTheShadow;
var
  LForm: TForm;
  LGroup: TStyledButtonGroup;
  LItem: TStyledGrpButtonItem;
  LPlain, LShadow: TBitmap;
  LMid: Integer;

  function BlueRows(const ABitmap: TBitmap): Integer;
  var
    Y: Integer;
  begin
    Result := 0;
    for Y := 0 to ABitmap.Height - 1 do
      if GetRValue(ColorToRGB(TStyledTestUtils.PixelAt(ABitmap, 8, Y))) < 120 then
        Inc(Result);
  end;

begin
  LForm := TStyledTestUtils.HostForm;
  try
    LGroup := TStyledButtonGroup.Create(LForm);
    LGroup.Parent := LForm;
    LGroup.SetBounds(0, 0, 200, 80);
    LGroup.StyleFamily := BOOTSTRAP_FAMILY;
    LGroup.StyleClass := btn_primary;
    LGroup.StyleAppearance := BOOTSTRAP_NORMAL;
    LItem := LGroup.Items.Add as TStyledGrpButtonItem;
    LItem.Caption := 'A';
    Assert.AreEqual(DEFAULT_SHADOW_SIZE, LItem.StyleShadowSize, 'default');
    LMid := LGroup.ButtonHeight div 2;
    LPlain := TStyledTestUtils.PaintWinControl(LGroup);
    try
      LGroup.StyleShadowSize := 4;
      Assert.AreEqual(4, LItem.StyleShadowSize, 'item inherits the group value');
      LShadow := TStyledTestUtils.PaintWinControl(LGroup);
      try
        //the face of the item (the blue of Primary) is shorter by the shadow at the
        //bottom, while the top is unchanged
        Assert.IsTrue(BlueRows(LShadow) < BlueRows(LPlain) - 2,
          Format('the face of the item is reduced at the bottom: %d rows against %d',
          [BlueRows(LShadow), BlueRows(LPlain)]));
        Assert.IsTrue(TStyledTestUtils.SameColor(TStyledTestUtils.PixelAt(LShadow, 8, 3),
          TStyledTestUtils.PixelAt(LPlain, 8, 3), 12), 'the top of the item is unchanged (up to the antialiasing)');
      finally
        LShadow.Free;
      end;
    finally
      LPlain.Free;
    end;
    Assert.IsTrue(Pos('StyleShadowSize = 4', TStyledTestUtils.DfmText(LGroup)) > 0, 'stored');
  finally
    LForm.Free;
  end;
end;

procedure TStyleShadowSizeTests.CategoryButtons_ItemInheritsAndPaintsTheShadow;
var
  LForm: TForm;
  LCategoryButtons: TStyledCategoryButtons;
  LCategory: TButtonCategory;
  LItem: TStyledButtonItem;
  LPlain, LShadow: TBitmap;
  X, Y: Integer;
  LDifferent: Boolean;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LCategoryButtons := TStyledCategoryButtons.Create(LForm);
    LCategoryButtons.Parent := LForm;
    LCategoryButtons.SetBounds(0, 0, 200, 120);
    LCategoryButtons.StyleFamily := BOOTSTRAP_FAMILY;
    LCategoryButtons.StyleClass := btn_primary;
    LCategoryButtons.StyleAppearance := BOOTSTRAP_NORMAL;
    LCategory := LCategoryButtons.Categories.Add;
    LCategory.Caption := 'Cat';
    LItem := LCategory.Items.Add as TStyledButtonItem;
    LItem.Caption := 'A';
    Assert.AreEqual(DEFAULT_SHADOW_SIZE, LItem.StyleShadowSize, 'default');
    LPlain := TStyledTestUtils.PaintWinControl(LCategoryButtons);
    try
      LCategoryButtons.StyleShadowSize := 4;
      Assert.AreEqual(4, LItem.StyleShadowSize, 'item inherits the container value');
      LShadow := TStyledTestUtils.PaintWinControl(LCategoryButtons);
      try
        LDifferent := False;
        for Y := 0 to LPlain.Height - 1 do
          for X := 0 to LPlain.Width - 1 do
            if TStyledTestUtils.PixelAt(LPlain, X, Y) <> TStyledTestUtils.PixelAt(LShadow, X, Y) then
              LDifferent := True;
        Assert.IsTrue(LDifferent, 'the item is painted with a shadow');
      finally
        LShadow.Free;
      end;
    finally
      LPlain.Free;
    end;
    Assert.IsTrue(Pos('StyleShadowSize = 4', TStyledTestUtils.DfmText(LCategoryButtons)) > 0, 'stored');
  finally
    LForm.Free;
  end;
end;

procedure TStyleShadowSizeTests.Panel_StyleShadowSize_PaintsStreamsAndAssigns;
var
  LForm: TForm;
  LPanel, LCopy, LOther: TStyledPanel;
  LBitmap: TBitmap;
  Y, LBelow: Integer;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LPanel := TStyledPanel.Create(LForm);
    LPanel.Parent := LForm;
    LPanel.SetBounds(0, 0, 200, 80);
    LPanel.StyleFamily := BOOTSTRAP_FAMILY;
    LPanel.StyleClass := btn_primary;
    LPanel.StyleAppearance := BOOTSTRAP_NORMAL;
    Assert.AreEqual(DEFAULT_SHADOW_SIZE, LPanel.StyleShadowSize, 'default = from the family');
    Assert.IsTrue(Pos('StyleShadowSize', TStyledTestUtils.DfmText(LPanel)) = 0, 'the default is not streamed');
    LPanel.StyleShadowSize := 4;
    Assert.AreEqual(4, LPanel.StyleShadowSize);
    LBitmap := PaintPanelFully(LPanel);
    try
      LBelow := 0;
      for Y := 50 to 79 do
        if (GetRValue(ColorToRGB(TStyledTestUtils.PixelAt(LBitmap, 100, Y))) < 250) and
          (GetRValue(ColorToRGB(TStyledTestUtils.PixelAt(LBitmap, 100, Y))) > 150) then
          Inc(LBelow);
      Assert.IsTrue(LBelow >= 2, Format('shadow rows below the face: %d', [LBelow]));
      //nothing is reduced at the top and at the sides
      Assert.IsTrue(TStyledTestUtils.PixelAt(LBitmap, 100, 0) <> clPureWhite, 'first row: the face');
      Assert.IsTrue(TStyledTestUtils.PixelAt(LBitmap, 3, 40) <> clPureWhite, 'first columns: the face');
    finally
      LBitmap.Free;
    end;
    Assert.IsTrue(Pos('StyleShadowSize = 4', TStyledTestUtils.DfmText(LPanel)) > 0, 'stored when not default');
    LCopy := TStyledTestUtils.StreamRoundTrip(LPanel, TStyledPanel, LForm) as TStyledPanel;
    Assert.AreEqual(4, LCopy.StyleShadowSize, 'survives streaming');
    LOther := TStyledPanel.Create(LForm);
    LOther.Assign(LPanel);
    Assert.AreEqual(4, LOther.StyleShadowSize, 'copied by Assign');
  finally
    LForm.Free;
  end;
end;

procedure TStyleShadowSizeTests.Panel_Elevated_FamilyShadow_ZeroRemovesIt;
var
  LForm: TForm;
  LPanel: TStyledPanel;
  LBitmap: TBitmap;
  LShadowEdge, LShadowFace, LFlatEdge, LFlatFace: TColor;
  LRaised: Boolean;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LPanel := TStyledPanel.Create(LForm);
    LPanel.Parent := LForm;
    LPanel.SetBounds(0, 0, 200, 80);
    LPanel.StyleFamily := ANGULAR_M3_LIGHT_FAMILY;
    LPanel.StyleClass := btn_Basic;
    LPanel.StyleAppearance := ElevatedAttr;
    LBitmap := PaintPanelFully(LPanel);
    try
      LShadowEdge := TStyledTestUtils.PixelAt(LBitmap, 100, 78);
      LShadowFace := TStyledTestUtils.PixelAt(LBitmap, 100, 40);
    finally
      LBitmap.Free;
    end;
    LPanel.StyleShadowSize := 0;
    LBitmap := PaintPanelFully(LPanel);
    try
      LFlatEdge := TStyledTestUtils.PixelAt(LBitmap, 100, 78);
      LFlatFace := TStyledTestUtils.PixelAt(LBitmap, 100, 40);
    finally
      LBitmap.Free;
    end;
    Assert.IsTrue(LFlatEdge = LFlatFace, 'no shadow: the face reaches the bottom edge');
    Assert.IsTrue(LShadowFace = LFlatFace, 'the face is the same');
    Assert.IsTrue(LShadowEdge <> LShadowFace, Format('the family shadow reduces the face: normal %d en %d col %s bc %s ds %d edge %s face %s', [LPanel.PanelStyleNormal.ShadowSize, Ord(LPanel.Enabled), TStyledTestUtils.ColorText(LPanel.Color), TStyledTestUtils.ColorText(LPanel.PanelStyleNormal.ButtonColor), Ord(LPanel.PanelStyleNormal.ButtonDrawStyle), TStyledTestUtils.ColorText(LShadowEdge), TStyledTestUtils.ColorText(LShadowFace)]));
    LRaised := False;
    try
      LPanel.StyleShadowSize := -2;
    except
      on EReadError do LRaised := True;
    end;
    Assert.IsTrue(LRaised, 'values below -1 are refused');
  finally
    LForm.Free;
  end;
end;

procedure TStyleShadowSizeTests.Panel_ChildrenStayInsideTheFace;
var
  LForm: TForm;
  LPanel: TStyledPanel;
  LChild: TPanel;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LPanel := TStyledPanel.Create(LForm);
    LPanel.Parent := LForm;
    LPanel.SetBounds(0, 0, 200, 80);
    LPanel.StyleFamily := BOOTSTRAP_FAMILY;
    LPanel.StyleClass := btn_primary;
    LPanel.StyleAppearance := BOOTSTRAP_NORMAL;
    LChild := TPanel.Create(LForm);
    LChild.Parent := LPanel;
    LChild.Align := alClient;
    LPanel.HandleNeeded;
    LPanel.Realign;
    Assert.AreEqual(0, LChild.Left, 'no shadow: the child fills the panel');
    LPanel.StyleShadowSize := 5;
    LPanel.Realign;
    Assert.AreEqual(0, LChild.Left, 'the shadow is only below: the left is unchanged');
    Assert.AreEqual(0, LChild.Top, 'the shadow is only below: the top is unchanged');
    Assert.AreEqual(LPanel.ClientWidth, LChild.Width, 'the width is unchanged');
    Assert.IsTrue(LChild.Top + LChild.Height <= LPanel.ClientHeight - 5, 'child above the shadow');
  finally
    LForm.Free;
  end;
end;

{ TStyledGroupItemTests }

procedure TStyledGroupItemTests.ButtonGroup_ItemAddedToEmptyGroup_InheritsContainerShape;
var
  LForm: TForm;
  LGroup: TStyledButtonGroup;
  LItem: TStyledGrpButtonItem;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LGroup := TStyledButtonGroup.Create(LForm);
    LGroup.Parent := LForm;
    LGroup.StyleDrawType := btEllipse;
    LGroup.StyleRadius := 17;
    LItem := LGroup.Items.Add as TStyledGrpButtonItem;
    LItem.Caption := 'A';
    Assert.IsTrue(LItem.StyleDrawType = btEllipse, 'item must inherit the group StyleDrawType');
    Assert.AreEqual(17, LItem.StyleRadius, 'item must inherit the group StyleRadius');
  finally
    LForm.Free;
  end;
end;

procedure TStyledGroupItemTests.CategoryButtons_ItemAddedToEmptyCategory_InheritsContainerShape;
var
  LForm: TForm;
  LCategoryButtons: TStyledCategoryButtons;
  LCategory: TButtonCategory;
  LItem: TStyledButtonItem;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LCategoryButtons := TStyledCategoryButtons.Create(LForm);
    LCategoryButtons.Parent := LForm;
    LCategoryButtons.StyleDrawType := btEllipse;
    LCategoryButtons.StyleRadius := 17;
    LCategory := LCategoryButtons.Categories.Add;
    LCategory.Caption := 'Cat';
    LItem := LCategory.Items.Add as TStyledButtonItem;
    LItem.Caption := 'A';
    Assert.IsTrue(LItem.StyleDrawType = btEllipse, 'item must inherit the container StyleDrawType');
    Assert.AreEqual(17, LItem.StyleRadius, 'item must inherit the container StyleRadius');
  finally
    LForm.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TStyledToolbarTests);
  TDUnitX.RegisterTestFixture(TStyledPanelTests);
  TDUnitX.RegisterTestFixture(TStyledGroupItemTests);
  TDUnitX.RegisterTestFixture(TStyleBorderWidthContainerTests);
  TDUnitX.RegisterTestFixture(TStyleShadowSizeTests);

end.
