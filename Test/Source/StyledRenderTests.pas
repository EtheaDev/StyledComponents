/// <summary>
///  The rendering engine: shape drawing, style-family registry and the
///  IStyledButtonAttributes contract every family must honour.
/// </summary>
unit StyledRenderTests;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  System.UITypes,
  Winapi.Windows,
  Vcl.Graphics,
  Vcl.Imaging.pngimage,
  DUnitX.TestFramework,
  Vcl.ButtonStylesAttributes,
  Vcl.StandardButtonStyles,
  StyledTestUtils;

const
  /// <summary>
  ///  A colour no family ever produces: whatever still holds it after
  ///  UpdateAttributes was never assigned by that family. Asymmetric and dark
  ///  on purpose: a near-white value would be a fixed point of
  ///  LightenColor(x, 50) (253 -> 254) and flag derived colours as unassigned.
  /// </summary>
  SENTINEL_COLOR = TColor($00010203);

type
  [TestFixture]
  TShapeDrawingTests = class
  public
    /// <summary>
    ///  Regression (4.2.2). GetRoundedCornersPath clamped the arc box to half
    ///  the short side; each corner only uses a quarter of its arc, so the
    ///  geometric limit is the whole short side. The clamp turned every
    ///  btRounded button from a pill (radius = height/2) into a round-rect with
    ///  radius = height/4.
    /// </summary>
    [Test]
    procedure Rounded_IsAPill_CornerRadiusIsHalfTheHeight;
  end;

  [TestFixture]
  TBadgeDrawingTests = class
  public
    /// <summary>
    ///  A notification badge is always a pill (btRounded), whatever Pen.Width
    ///  the shared canvas carries from the border just drawn. PNG dumps go to
    ///  %STYLED_BADGE_DUMP% when set (diagnostic).
    /// </summary>
    [Test]
    [TestCase('PenWidth1', '1')]
    [TestCase('PenWidth3', '3')]
    [TestCase('PenWidth6', '6')]
    procedure Badge_IsAPill_WhateverThePenWidth(const APenWidth: Integer);
  end;

  [TestFixture]
  TStyleFamilyRegistryTests = class
  public
    /// <summary>Sanity: the families compiled in (Classic, Bootstrap, Angular x2, Colors x2) are registered.</summary>
    [Test]
    procedure Families_AreRegistered;

    /// <summary>Sanity: every registered family lists at least one class and one appearance.</summary>
    [Test]
    [AllFamilies]
    procedure Family_HasClassesAndAppearances(const AFamily: string);
  end;

  [TestFixture]
  TStyleFamilyContractTests = class
  private
    procedure Prime(const AAttributes: array of TStyledButtonAttributes);
    function MaxChannelDelta(const A, B: TColor): Integer;
  public
    /// <summary>
    ///  Regression. The Outline appearance of Bootstrap, Basic-Colors, SVG-Colors
    ///  and the Template family never assigns Normal.ButtonColor, so it stays at
    ///  the constructor default (clBlack). Consumers read ButtonColor regardless
    ///  of ButtonDrawStyle (TStyledPanel.Color, TStyledAnimatedButton erase,
    ///  the AutoClick progress bar) and paint black. Classic assigns it.
    /// </summary>
    [Test]
    [AllFamilies]
    procedure UpdateAttributes_EveryClassAndAppearance_AssignsButtonAndFontColour(const AFamily: string);

    /// <summary>
    ///  Regression. Angular Stroked sets a solid 2px border but never its colour,
    ///  so every stroked button (Disabled included) gets a clBlack frame.
    /// </summary>
    [Test]
    [AllFamilies]
    procedure UpdateAttributes_SolidBorder_AssignsBorderColour(const AFamily: string);

    /// <summary>Guard: GetStyleByModalResult always answers with a listed class and appearance.</summary>
    [Test]
    [AllFamilies]
    procedure GetStyleByModalResult_ReturnsListedClassAndAppearance(const AFamily: string);

    /// <summary>
    ///  Regression. Classic derives Disabled from ThemeType (ttDark => darken the
    ///  font) instead of from the colours themselves; a dark style with a dark
    ///  font on a light button (Obsidian, Onyx Blue, Vapor, Stellar) gets a
    ///  Disabled state indistinguishable from Normal.
    /// </summary>
    [Test]
    procedure Classic_Disabled_VisiblyDiffersFromNormal_ForEveryStyle;

    /// <summary>
    ///  Regression. GetButtonClasses caches the class list on first use and
    ///  RegisterThemeAttributes never invalidates it: a VCL style registered
    ///  after the first styled control resolved its style is not selectable.
    /// </summary>
    [Test]
    procedure Classic_RegisterThemeAttributes_AfterFirstUse_IsSelectable;
  end;

implementation

const
  STATE_NAMES: array[0..4] of string = ('Normal', 'Pressed', 'Selected', 'Hot', 'Disabled');
  MODAL_RESULTS: array[0..14] of TModalResult = (mrNone, mrOk, mrCancel, mrAbort,
    mrRetry, mrIgnore, mrYes, mrNo, mrAll, mrNoToAll, mrYesToAll, mrClose,
    mrTryAgain, mrContinue, mrHelp);

function IndexOfText(const AList: array of string; const AValue: string): Integer;
var
  I: Integer;
begin
  for I := 0 to High(AList) do
    if SameText(AList[I], AValue) then
      Exit(I);
  Result := -1;
end;

{ TShapeDrawingTests }

procedure TShapeDrawingTests.Rounded_IsAPill_CornerRadiusIsHalfTheHeight;
var
  LBitmap: TBitmap;
begin
  // 200x60 pill: the correct corner circle is centred at (29.5, 29.5) r=29.5,
  // the over-clamped one at (15, 15) r=15. Pixel (6,6) lies 3.7 px outside the
  // first and 2.3 px inside the second, so it is white for a pill and black for
  // the round-rect the regression draws.
  LBitmap := TStyledTestUtils.NewBitmap(200, 60, clPureWhite);
  try
    LBitmap.Canvas.Pen.Width := 1;
    LBitmap.Canvas.Pen.Color := clPureBlack;
    LBitmap.Canvas.Brush.Color := clPureBlack;
    CanvasDrawShape(LBitmap.Canvas, Rect(0, 0, 200, 60), btRounded, 0, ALL_ROUNDED_CORNERS);
    TStyledTestUtils.AssertPixel(LBitmap, 100, 30, clPureBlack, 'centre of the pill');
    TStyledTestUtils.AssertPixel(LBitmap, 6, 6, clPureWhite, 'outside the pill corner');
  finally
    LBitmap.Free;
  end;
end;

{ TBadgeDrawingTests }

procedure TBadgeDrawingTests.Badge_IsAPill_WhateverThePenWidth(const APenWidth: Integer);
var
  LBitmap: TBitmap;
  LPng: TPngImage;
  LDump: string;
  LBadgeH, LBadgeW, X, Y, R: Integer;
  LRow, LCol: Integer;
begin
  LBitmap := TStyledTestUtils.NewBitmap(160, 60, clPureWhite);
  try
    LBitmap.Canvas.Font.Name := 'Segoe UI';
    LBitmap.Canvas.Font.Size := 9;
    LBitmap.Canvas.Pen.Width := APenWidth;
    LBitmap.Canvas.Pen.Color := clPureBlack;
    DrawButtonNotificationBadge(LBitmap.Canvas, Rect(0, 0, 160, 60), 1.0, '12',
      nbsNormal, nbpTopLeft, clPureBlack, clPureWhite, [fsBold]);
    LDump := GetEnvironmentVariable('STYLED_BADGE_DUMP');
    if LDump <> '' then
    begin
      ForceDirectories(LDump);
      LPng := TPngImage.Create;
      try
        LPng.Assign(LBitmap);
        LPng.SaveToFile(IncludeTrailingPathDelimiter(LDump) + Format('badge_pen%d.png', [APenWidth]));
      finally
        LPng.Free;
      end;
    end;
    // Badge height: the longest dark run found in any column (the text leaves
    // white holes, the caps shorten the outer columns, the full one is in between)
    LBadgeH := 0;
    for X := 0 to 40 do
    begin
      LCol := 0;
      for LRow := 0 to 59 do
        if TStyledTestUtils.SameColor(TStyledTestUtils.PixelAt(LBitmap, X, LRow), clPureBlack, 60) then
          Inc(LCol);
      LBadgeH := Max(LBadgeH, LCol);
    end;
    Assert.IsTrue(LBadgeH >= 12, Format('badge drawn (height %d)', [LBadgeH]));
    LBadgeW := 0;
    for X := 0 to 159 do
      if TStyledTestUtils.SameColor(TStyledTestUtils.PixelAt(LBitmap, X, LBadgeH div 2), clPureBlack, 60) then
        Inc(LBadgeW);
    // A pill has a corner circle of radius H/2 centred at (R, R): the pixel on the
    // diagonal at 15% of R from the corner is outside a pill, inside a round-rect.
    R := LBadgeH div 2;
    X := Round(R * 0.15);
    Y := X;
    Assert.IsTrue(TStyledTestUtils.SameColor(TStyledTestUtils.PixelAt(LBitmap, X, Y), clPureWhite, 60),
      Format('pen %d: badge %dx%d, pixel (%d,%d) is %s: a pill leaves it white', [APenWidth,
        LBadgeW, LBadgeH, X, Y, TStyledTestUtils.ColorText(ColorToRGB(TStyledTestUtils.PixelAt(LBitmap, X, Y)))]));
  finally
    LBitmap.Free;
  end;
end;

{ TStyleFamilyRegistryTests }

procedure TStyleFamilyRegistryTests.Families_AreRegistered;
begin
  Assert.IsTrue(Length(TStyledTestUtils.FamilyNames) >= 6,
    Format('%d families registered', [Length(TStyledTestUtils.FamilyNames)]));
  Assert.IsTrue(StyleFamilyExists('Classic'), 'Classic family');
  Assert.IsTrue(StyleFamilyExists('Bootstrap'), 'Bootstrap family');
end;

procedure TStyleFamilyRegistryTests.Family_HasClassesAndAppearances(const AFamily: string);
begin
  Assert.IsTrue(Length(GetButtonFamilyClasses(AFamily)) > 0, AFamily + ': classes');
  Assert.IsTrue(Length(GetButtonFamilyAppearances(AFamily)) > 0, AFamily + ': appearances');
end;

{ TStyleFamilyContractTests }

procedure TStyleFamilyContractTests.Prime(const AAttributes: array of TStyledButtonAttributes);
var
  I: Integer;
begin
  for I := 0 to High(AAttributes) do
  begin
    AAttributes[I].ButtonColor := SENTINEL_COLOR;
    AAttributes[I].BorderColor := SENTINEL_COLOR;
    AAttributes[I].FontColor := SENTINEL_COLOR;
  end;
end;

function TStyleFamilyContractTests.MaxChannelDelta(const A, B: TColor): Integer;
var
  LA, LB: TColor;
  LDelta: Integer;
begin
  LA := ColorToRGB(A);
  LB := ColorToRGB(B);
  Result := Abs(GetRValue(LA) - GetRValue(LB));
  LDelta := Abs(GetGValue(LA) - GetGValue(LB));
  if LDelta > Result then
    Result := LDelta;
  LDelta := Abs(GetBValue(LA) - GetBValue(LB));
  if LDelta > Result then
    Result := LDelta;
end;

procedure TStyleFamilyContractTests.UpdateAttributes_EveryClassAndAppearance_AssignsButtonAndFontColour(
  const AFamily: string);
var
  LFamily: TButtonFamily;
  LClasses: TButtonClasses;
  LAppearances: TButtonAppearances;
  LClass, LAppearance: string;
  N, P, S, H, D: TStyledButtonAttributes;
  LStates: array[0..4] of TStyledButtonAttributes;
  LFailures: TStringList;
  I: Integer;
begin
  LFamily := GetButtonFamilyClass(AFamily);
  Assert.IsNotNull(LFamily, AFamily);
  LClasses := GetButtonFamilyClasses(AFamily);
  LAppearances := GetButtonFamilyAppearances(AFamily);
  TStyledTestUtils.NewAttributes(N, P, S, H, D);
  LFailures := TStringList.Create;
  try
    LStates[0] := N; LStates[1] := P; LStates[2] := S; LStates[3] := H; LStates[4] := D;
    for LClass in LClasses do
      for LAppearance in LAppearances do
      begin
        Prime(LStates);
        LFamily.StyledAttributes.UpdateAttributes(AFamily, LClass, LAppearance, N, P, S, H, D);
        for I := 0 to 4 do
        begin
          if LStates[I].ButtonColor = SENTINEL_COLOR then
            LFailures.Add(Format('%s / %s: %s.ButtonColor not assigned', [LClass, LAppearance, STATE_NAMES[I]]));
          if LStates[I].FontColor = SENTINEL_COLOR then
            LFailures.Add(Format('%s / %s: %s.FontColor not assigned', [LClass, LAppearance, STATE_NAMES[I]]));
        end;
      end;
    Assert.AreEqual(0, LFailures.Count,
      Format('%s: %d unassigned colours', [AFamily, LFailures.Count]) + sLineBreak + LFailures.Text);
  finally
    LFailures.Free;
    TStyledTestUtils.FreeAttributes(N, P, S, H, D);
  end;
end;

procedure TStyleFamilyContractTests.UpdateAttributes_SolidBorder_AssignsBorderColour(
  const AFamily: string);
var
  LFamily: TButtonFamily;
  LClasses: TButtonClasses;
  LAppearances: TButtonAppearances;
  LClass, LAppearance: string;
  N, P, S, H, D: TStyledButtonAttributes;
  LStates: array[0..4] of TStyledButtonAttributes;
  LFailures: TStringList;
  I: Integer;
begin
  LFamily := GetButtonFamilyClass(AFamily);
  Assert.IsNotNull(LFamily, AFamily);
  LClasses := GetButtonFamilyClasses(AFamily);
  LAppearances := GetButtonFamilyAppearances(AFamily);
  TStyledTestUtils.NewAttributes(N, P, S, H, D);
  LFailures := TStringList.Create;
  try
    LStates[0] := N; LStates[1] := P; LStates[2] := S; LStates[3] := H; LStates[4] := D;
    for LClass in LClasses do
      for LAppearance in LAppearances do
      begin
        Prime(LStates);
        LFamily.StyledAttributes.UpdateAttributes(AFamily, LClass, LAppearance, N, P, S, H, D);
        for I := 0 to 4 do
          if (LStates[I].BorderDrawStyle = brdSolid) and (LStates[I].BorderWidth > 0) and
             (LStates[I].BorderColor = SENTINEL_COLOR) then
            LFailures.Add(Format('%s / %s: %s has a solid border without a colour',
              [LClass, LAppearance, STATE_NAMES[I]]));
      end;
    Assert.AreEqual(0, LFailures.Count,
      Format('%s: %d solid borders without colour', [AFamily, LFailures.Count]) + sLineBreak + LFailures.Text);
  finally
    LFailures.Free;
    TStyledTestUtils.FreeAttributes(N, P, S, H, D);
  end;
end;

procedure TStyleFamilyContractTests.GetStyleByModalResult_ReturnsListedClassAndAppearance(
  const AFamily: string);
var
  LFamily: TButtonFamily;
  LClasses: TButtonClasses;
  LAppearances: TButtonAppearances;
  LClass, LAppearance: string;
  LModalResult: TModalResult;
begin
  LFamily := GetButtonFamilyClass(AFamily);
  Assert.IsNotNull(LFamily, AFamily);
  LClasses := GetButtonFamilyClasses(AFamily);
  LAppearances := GetButtonFamilyAppearances(AFamily);
  for LModalResult in MODAL_RESULTS do
  begin
    LClass := '';
    LAppearance := '';
    LFamily.StyledAttributes.GetStyleByModalResult(LModalResult, LClass, LAppearance);
    Assert.IsTrue(IndexOfText(LClasses, LClass) >= 0,
      Format('%s: ModalResult %d -> class "%s" is not listed', [AFamily, LModalResult, LClass]));
    Assert.IsTrue(IndexOfText(LAppearances, LAppearance) >= 0,
      Format('%s: ModalResult %d -> appearance "%s" is not listed', [AFamily, LModalResult, LAppearance]));
  end;
end;

procedure TStyleFamilyContractTests.Classic_Disabled_VisiblyDiffersFromNormal_ForEveryStyle;
const
  MIN_DELTA = 40; // per channel: a 10% tint stays well below it, a 50% one well above
var
  LFamily: TButtonFamily;
  LClasses: TButtonClasses;
  LClass: string;
  N, P, S, H, D: TStyledButtonAttributes;
  LFailures: TStringList;
begin
  LFamily := GetButtonFamilyClass(DEFAULT_CLASSIC_FAMILY);
  Assert.IsNotNull(LFamily);
  LClasses := GetButtonFamilyClasses(DEFAULT_CLASSIC_FAMILY);
  // Without a comctl32 v6 manifest StyleServices.Enabled is False and the
  // VCL-style table is not registered: the loop below would then be vacuous.
  Assert.IsTrue(Length(LClasses) > 10,
    Format('VCL style table registered (manifest): %d classes', [Length(LClasses)]));
  TStyledTestUtils.NewAttributes(N, P, S, H, D);
  LFailures := TStringList.Create;
  try
    for LClass in LClasses do
    begin
      LFamily.StyledAttributes.UpdateAttributes(DEFAULT_CLASSIC_FAMILY, LClass, DEFAULT_APPEARANCE, N, P, S, H, D);
      if (MaxChannelDelta(N.FontColor, D.FontColor) < MIN_DELTA) and
         (MaxChannelDelta(N.ButtonColor, D.ButtonColor) < MIN_DELTA) then
        LFailures.Add(Format('%s: Normal font %s / button %s, Disabled font %s / button %s',
          [LClass, TStyledTestUtils.ColorText(ColorToRGB(N.FontColor)),
           TStyledTestUtils.ColorText(ColorToRGB(N.ButtonColor)),
           TStyledTestUtils.ColorText(ColorToRGB(D.FontColor)),
           TStyledTestUtils.ColorText(ColorToRGB(D.ButtonColor))]));
    end;
    Assert.AreEqual(0, LFailures.Count,
      Format('%d Classic styles whose Disabled state looks like Normal', [LFailures.Count]) +
      sLineBreak + LFailures.Text);
  finally
    LFailures.Free;
    TStyledTestUtils.FreeAttributes(N, P, S, H, D);
  end;
end;

procedure TStyleFamilyContractTests.Classic_RegisterThemeAttributes_AfterFirstUse_IsSelectable;
const
  STYLE_NAME = 'UnitTest Late Style';
var
  LClass, LAppearance: string;
  LFamily: TButtonFamily;
begin
  // First use builds the cached class list
  Assert.IsTrue(Length(GetButtonFamilyClasses(DEFAULT_CLASSIC_FAMILY)) > 0);
  RegisterThemeAttributes(STYLE_NAME, ttLight, clBlack, clBlack, clWhite, clWhite,
    clSilver, clGray, clGray, btRoundRect);
  Assert.IsTrue(IndexOfText(GetButtonFamilyClasses(DEFAULT_CLASSIC_FAMILY), STYLE_NAME) >= 0,
    'a style registered after the first use must be listed');
  LClass := STYLE_NAME;
  LAppearance := DEFAULT_APPEARANCE;
  Assert.IsTrue(StyleFamilyCheckAttributes(DEFAULT_CLASSIC_FAMILY, LClass, LAppearance, LFamily),
    'a style registered after the first use must be selectable');
  Assert.AreEqual(STYLE_NAME, LClass, 'CheckAttributes must not fall back to Windows');
end;

initialization
  TDUnitX.RegisterTestFixture(TShapeDrawingTests);
  TDUnitX.RegisterTestFixture(TBadgeDrawingTests);
  TDUnitX.RegisterTestFixture(TStyleFamilyRegistryTests);
  TDUnitX.RegisterTestFixture(TStyleFamilyContractTests);

end.
