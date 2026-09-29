/// <summary>
///  The container components: toolbar, navigators, button group, category buttons, panel.
/// </summary>
unit StyledContainerTests;

interface

uses
  System.SysUtils,
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
  Vcl.StyledToolbar,
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

end.
