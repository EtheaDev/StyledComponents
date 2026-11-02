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
  Vcl.Forms,
  Vcl.Controls,
  Vcl.AngularButtonStyles,
  Vcl.BootstrapButtonStyles,
  Vcl.StyledButton,
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
  TAngularM3Tests = class
  private
    procedure Resolve(const AFamily, AClass, AAppearance: string;
      out N, P, S, H, D: TStyledButtonAttributes);
  public
    /// <summary>Colours of the azure palette (Angular Material 3 light scheme).</summary>
    [Test]
    procedure Light_Filled_UsesTheAzurePrimary;
    /// <summary>Colours of the azure palette (Angular Material 3 dark scheme).</summary>
    [Test]
    procedure Dark_Filled_UsesTheAzurePrimary;
    /// <summary>Every M3 button is a "full corner" pill, in both schemes and every appearance.</summary>
    [Test]
    [TestCase('Light', 'Angular-M3-Light')]
    [TestCase('Dark', 'Angular-M3-Dark')]
    procedure EveryAppearance_IsRounded(const AFamily: string);
    /// <summary>Hot, Pressed and Disabled must be visibly different from Normal.</summary>
    [Test]
    [TestCase('Light', 'Angular-M3-Light')]
    [TestCase('Dark', 'Angular-M3-Dark')]
    procedure EveryAppearance_StatesDifferFromNormal(const AFamily: string);
    /// <summary>State layers of Angular Material 3: hover 8%, pressed (layer + ripple) about 22%, so Pressed is clearly darker than Hot.</summary>
    [Test]
    [TestCase('Light', 'Angular-M3-Light')]
    [TestCase('Dark', 'Angular-M3-Dark')]
    procedure Pressed_IsClearlyStrongerThanHot(const AFamily: string);
    /// <summary>Only Outlined has a border: the Elevated button has a shadow instead.</summary>
    [Test]
    procedure OnlyOutlined_HasABorder;
    /// <summary>Elevated has a shadow (bigger when Hot, none when Disabled), the other appearances do not.</summary>
    [Test]
    [TestCase('Light', 'Angular-M3-Light')]
    [TestCase('Dark', 'Angular-M3-Dark')]
    procedure Elevated_HasAShadow_OthersDoNot(const AFamily: string);
    /// <summary>The shadow is drawn inside the area of the control, around the face of the button.</summary>
    [Test]
    procedure Elevated_PaintsTheShadowInsideTheControl;
    /// <summary>StyleShadowSize: default is "from the family" (-1) and is not stored.</summary>
    [Test]
    procedure StyleShadowSize_Default_IsTheFamilyAndNotStored;
    /// <summary>StyleShadowSize on a Classic button paints a shadow inside the control and is streamed and assigned.</summary>
    [Test]
    procedure StyleShadowSize_OnAClassicButton_PaintsStreamsAndAssigns;
    /// <summary>StyleShadowSize = 0 removes the shadow of Elevated; values below -1 are refused.</summary>
    [Test]
    procedure StyleShadowSizeZero_RemovesTheShadowOfElevated;
    /// <summary>Without a shadow the face reaches the edge of the control, as before.</summary>
    [Test]
    procedure Filled_FaceReachesTheEdgeOfTheControl;
    /// <summary>A single class: Basic (Link and Disabled of the page are not different buttons).</summary>
    [Test]
    procedure Classes_IsOnlyBasic;
    /// <summary>A button of the family is a pill by default, painted as a pill.</summary>
    [Test]
    procedure Button_DefaultsToRoundedAndPaintsAPill;
  end;

  [TestFixture]
  TStyleBorderWidthTests = class
  public
    /// <summary>The default (-1) is the border of the style and is not streamed.</summary>
    [Test]
    procedure Default_IsTheStyleAndNotStored;
    /// <summary>A width greater than zero replaces the border of the style in every state.</summary>
    [Test]
    procedure CustomWidth_ReplacesTheBorderOfTheStyleInEveryState;
    /// <summary>0 removes the border; values below -1 are refused.</summary>
    [Test]
    procedure Zero_RemovesTheBorder;
    /// <summary>The value is streamed and copied by Assign.</summary>
    [Test]
    procedure CustomWidth_StreamsAndAssigns;
    /// <summary>The registered default applies to new buttons, also AsVCLComponent ones; the button value wins.</summary>
    [Test]
    procedure RegisteredDefault_AppliesToNewButtons;
  end;

  [TestFixture]
  TTransparentCaptionTests = class
  public
    /// <summary>
    ///  A Transparent button does not draw its face in the Normal state: the caption is
    ///  drawn on the parent, so it must contrast with the parent even when the font of
    ///  the style was chosen to contrast with the face (white on a dark face).
    /// </summary>
    [Test]
    [TestCase('BootstrapPrimaryOnWhite', 'Bootstrap,Primary,Normal,16777215')]
    [TestCase('BootstrapLightOnBlack', 'Bootstrap,Light,Normal,0')]
    [TestCase('ClassicOnBlack', 'Classic,Windows,Normal,0')]
    procedure Caption_ContrastsWithTheParent(const AFamily, AClass, AAppearance: string;
      const AParentColor: Integer);
    /// <summary>A font that already contrasts with the parent is kept as the style defines it.</summary>
    [Test]
    procedure ReadableFont_IsKept;
    /// <summary>A disabled Transparent button keeps its faded caption: it must not look enabled.</summary>
    [Test]
    [TestCase('ClassicOnWhite', 'Classic,Windows,Normal,16777215')]
    [TestCase('BootstrapPrimaryOutlineOnWhite', 'Bootstrap,Primary,Outline,16777215')]
    procedure DisabledCaption_StaysFaded(const AFamily, AClass, AAppearance: string;
      const AParentColor: Integer);
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

{ TAngularM3Tests }

procedure TAngularM3Tests.Resolve(const AFamily, AClass, AAppearance: string;
  out N, P, S, H, D: TStyledButtonAttributes);
var
  LClass, LAppearance: string;
begin
  TStyledTestUtils.NewAttributes(N, P, S, H, D);
  LClass := AClass;
  LAppearance := AAppearance;
  StyleFamilyUpdateAttributes(AFamily, LClass, LAppearance, N, P, S, H, D);
  Assert.AreEqual(AClass, LClass, 'class kept');
  Assert.AreEqual(AAppearance, LAppearance, 'appearance kept');
end;

procedure TAngularM3Tests.Light_Filled_UsesTheAzurePrimary;
var
  N, P, S, H, D: TStyledButtonAttributes;
begin
  Resolve(ANGULAR_M3_LIGHT_FAMILY, btn_Basic, FilledAttr, N, P, S, H, D);
  try
    Assert.AreEqual(Integer($BB5C00), Integer(ColorToRGB(N.ButtonColor)), 'Filled container = primary (#005CBB)');
    Assert.AreEqual(Integer($FFFFFF), Integer(ColorToRGB(N.FontColor)), 'Filled label = on-primary');
  finally
    TStyledTestUtils.FreeAttributes(N, P, S, H, D);
  end;
end;

procedure TAngularM3Tests.Dark_Filled_UsesTheAzurePrimary;
var
  N, P, S, H, D: TStyledButtonAttributes;
begin
  Resolve(ANGULAR_M3_DARK_FAMILY, btn_Basic, FilledAttr, N, P, S, H, D);
  try
    Assert.AreEqual(Integer($FFC7AB), Integer(ColorToRGB(N.ButtonColor)), 'Filled container = primary (#ABC7FF)');
    Assert.AreEqual(Integer($652F00), Integer(ColorToRGB(N.FontColor)), 'Filled label = on-primary (#002F65)');
  finally
    TStyledTestUtils.FreeAttributes(N, P, S, H, D);
  end;
end;

procedure TAngularM3Tests.EveryAppearance_IsRounded(const AFamily: string);
var
  LAppearance: string;
  N, P, S, H, D: TStyledButtonAttributes;
begin
  for LAppearance in GetButtonFamilyAppearances(AFamily) do
  begin
    Resolve(AFamily, btn_Basic, LAppearance, N, P, S, H, D);
    try
      Assert.AreEqual(Ord(btRounded), Ord(N.DrawType), AFamily + '/' + LAppearance);
    finally
      TStyledTestUtils.FreeAttributes(N, P, S, H, D);
    end;
  end;
end;

procedure TAngularM3Tests.EveryAppearance_StatesDifferFromNormal(const AFamily: string);
var
  LAppearance: string;
  N, P, S, H, D: TStyledButtonAttributes;

  function Differs(const A, B: TStyledButtonAttributes): Boolean;
  begin
    Result := (ColorToRGB(A.ButtonColor) <> ColorToRGB(B.ButtonColor)) or
      (ColorToRGB(A.FontColor) <> ColorToRGB(B.FontColor)) or
      (A.ButtonDrawStyle <> B.ButtonDrawStyle) or
      (ColorToRGB(A.BorderColor) <> ColorToRGB(B.BorderColor));
  end;

begin
  for LAppearance in GetButtonFamilyAppearances(AFamily) do
  begin
    Resolve(AFamily, btn_Basic, LAppearance, N, P, S, H, D);
    try
      Assert.IsTrue(Differs(H, N), AFamily + '/' + LAppearance + ': Hot looks like Normal');
      Assert.IsTrue(Differs(P, N), AFamily + '/' + LAppearance + ': Pressed looks like Normal');
      Assert.IsTrue(Differs(D, N), AFamily + '/' + LAppearance + ': Disabled looks like Normal');
    finally
      TStyledTestUtils.FreeAttributes(N, P, S, H, D);
    end;
  end;
end;

procedure TAngularM3Tests.Pressed_IsClearlyStrongerThanHot(const AFamily: string);
var
  LAppearance: string;
  N, P, S, H, D: TStyledButtonAttributes;

  function Distance(const A, B: TColor): Integer;
  begin
    Result := Abs(GetRValue(ColorToRGB(A)) - GetRValue(ColorToRGB(B))) +
      Abs(GetGValue(ColorToRGB(A)) - GetGValue(ColorToRGB(B))) +
      Abs(GetBValue(ColorToRGB(A)) - GetBValue(ColorToRGB(B)));
  end;

begin
  for LAppearance in GetButtonFamilyAppearances(AFamily) do
  begin
    Resolve(AFamily, btn_Basic, LAppearance, N, P, S, H, D);
    try
      Assert.IsTrue(Distance(P.ButtonColor, N.ButtonColor) > Distance(H.ButtonColor, N.ButtonColor) * 2,
        AFamily + '/' + LAppearance + ': Pressed must be much stronger than Hot');
      Assert.IsTrue(Distance(P.ButtonColor, N.ButtonColor) > Distance(S.ButtonColor, N.ButtonColor),
        AFamily + '/' + LAppearance + ': Pressed stronger than Focus');
    finally
      TStyledTestUtils.FreeAttributes(N, P, S, H, D);
    end;
  end;
end;

procedure TAngularM3Tests.OnlyOutlined_HasABorder;
var
  LAppearance: string;
  N, P, S, H, D: TStyledButtonAttributes;
begin
  for LAppearance in GetButtonFamilyAppearances(ANGULAR_M3_LIGHT_FAMILY) do
  begin
    Resolve(ANGULAR_M3_LIGHT_FAMILY, btn_Basic, LAppearance, N, P, S, H, D);
    try
      if SameText(LAppearance, OutlinedAttr) then
      begin
        Assert.AreEqual(Ord(brdSolid), Ord(N.BorderDrawStyle), LAppearance + ' border');
        Assert.AreEqual(1, N.BorderWidth, LAppearance + ' border width');
      end
      else
        Assert.AreEqual(Ord(brdClear), Ord(N.BorderDrawStyle), LAppearance + ' has no border');
    finally
      TStyledTestUtils.FreeAttributes(N, P, S, H, D);
    end;
  end;
end;

procedure TAngularM3Tests.Elevated_HasAShadow_OthersDoNot(const AFamily: string);
var
  LAppearance: string;
  N, P, S, H, D: TStyledButtonAttributes;
begin
  for LAppearance in GetButtonFamilyAppearances(AFamily) do
  begin
    Resolve(AFamily, btn_Basic, LAppearance, N, P, S, H, D);
    try
      if SameText(LAppearance, ElevatedAttr) or SameText(LAppearance, FabAttr) then
      begin
        Assert.IsTrue(N.ShadowSize > 0, 'Normal has a shadow');
        Assert.IsTrue(H.ShadowSize > N.ShadowSize, 'Hot: higher elevation, bigger shadow');
        Assert.IsTrue(P.ShadowSize <= H.ShadowSize, 'Pressed does not exceed Hot');
        Assert.IsTrue(S.ShadowSize > 0, 'Selected (focus) has a shadow');
        Assert.AreEqual(0, D.ShadowSize, 'Disabled is flat');
      end
      else
      begin
        Assert.AreEqual(0, N.ShadowSize, AFamily + '/' + LAppearance + ' Normal');
        Assert.AreEqual(0, H.ShadowSize, AFamily + '/' + LAppearance + ' Hot');
      end;
    finally
      TStyledTestUtils.FreeAttributes(N, P, S, H, D);
    end;
  end;
end;

procedure TAngularM3Tests.StyleShadowSize_Default_IsTheFamilyAndNotStored;
var
  LForm: TForm;
  LButton: TStyledButton;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LButton := TStyledButton.CreateStyled(LForm, ANGULAR_M3_LIGHT_FAMILY, btn_Basic, ElevatedAttr);
    Assert.AreEqual(DEFAULT_SHADOW_SIZE, LButton.StyleShadowSize, 'default = from the family');
    Assert.AreEqual(-1, DEFAULT_SHADOW_SIZE);
    Assert.IsTrue(Pos('StyleShadowSize', TStyledTestUtils.DfmText(LButton)) = 0,
      'the default is not streamed');
    //the family value is the one in the state attributes
    Assert.IsTrue(LButton.ButtonStyleNormal.ShadowSize > 0, 'Elevated: shadow from the family');
  finally
    LForm.Free;
  end;
end;

procedure TAngularM3Tests.StyleShadowSize_OnAClassicButton_PaintsStreamsAndAssigns;
var
  LForm: TForm;
  LButton, LCopy, LOther: TStyledButton;
  LBitmap: TBitmap;
  Y, LBelow: Integer;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LButton := TStyledButton.CreateStyled(LForm, DEFAULT_CLASSIC_FAMILY, DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE);
    LButton.Parent := LForm;
    LButton.SetBounds(0, 0, 200, 60);
    LButton.StyleShadowSize := 4;
    Assert.AreEqual(4, LButton.StyleShadowSize);
    LBitmap := TStyledTestUtils.PaintWinControl(LButton);
    try
      LBelow := 0;
      for Y := 40 to 59 do
        if (GetRValue(ColorToRGB(TStyledTestUtils.PixelAt(LBitmap, 100, Y))) < 250) and
          (GetRValue(ColorToRGB(TStyledTestUtils.PixelAt(LBitmap, 100, Y))) > 150) then
          Inc(LBelow);
      Assert.IsTrue(LBelow >= 2, Format('shadow rows below the face: %d', [LBelow]));
      //the control is not reduced at the top: the border starts at the first row
      Assert.IsTrue(TStyledTestUtils.PixelAt(LBitmap, 100, 0) <> clPureWhite, 'first row: the face');
    finally
      LBitmap.Free;
    end;
    //streamed
    Assert.IsTrue(Pos('StyleShadowSize = 4', TStyledTestUtils.DfmText(LButton)) > 0, 'stored when not default');
    LCopy := TStyledTestUtils.StreamRoundTrip(LButton, TStyledButton, LForm) as TStyledButton;
    Assert.AreEqual(4, LCopy.StyleShadowSize, 'survives streaming');
    //assigned
    LOther := TStyledButton.Create(LForm);
    LOther.Assign(LButton);
    Assert.AreEqual(4, LOther.StyleShadowSize, 'copied by Assign');
  finally
    LForm.Free;
  end;
end;

procedure TAngularM3Tests.StyleShadowSizeZero_RemovesTheShadowOfElevated;
var
  LForm: TForm;
  LButton: TStyledButton;
  LBitmap: TBitmap;
  LRaised: Boolean;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LButton := TStyledButton.CreateStyled(LForm, ANGULAR_M3_LIGHT_FAMILY, btn_Basic, ElevatedAttr);
    LButton.Parent := LForm;
    LButton.SetBounds(0, 0, 200, 60);
    LButton.StyleShadowSize := 0;
    Assert.AreEqual(0, LButton.StyleShadowSize);
    LBitmap := TStyledTestUtils.PaintWinControl(LButton);
    try
      //no shadow: the face reaches the edge of the control
      TStyledTestUtils.AssertPixel(LBitmap, 100, 57, $FDF9FA, 'face reaches the bottom edge');
    finally
      LBitmap.Free;
    end;
    LRaised := False;
    try
      LButton.StyleShadowSize := -2;
    except
      on EReadError do LRaised := True;
    end;
    Assert.IsTrue(LRaised, 'values below -1 are refused like StyleRadius <= 0');
  finally
    LForm.Free;
  end;
end;

procedure TAngularM3Tests.Elevated_PaintsTheShadowInsideTheControl;
var
  LForm: TForm;
  LButton: TStyledButton;
  LBitmap: TBitmap;
  X, Y, LBelow, LAbove, LLeft: Integer;

  function IsShadow(const AColor: TColor): Boolean;
  begin
    //darker than the (white) parent, but neither the face ($F4) nor black
    Result := (GetRValue(ColorToRGB(AColor)) < 240) and
      (GetRValue(ColorToRGB(AColor)) > 150);
  end;

begin
  LForm := TStyledTestUtils.HostForm;
  try
    LButton := TStyledButton.CreateStyled(LForm, ANGULAR_M3_LIGHT_FAMILY, btn_Basic, ElevatedAttr);
    LButton.Parent := LForm;
    LButton.SetBounds(0, 0, 200, 60);
    LBitmap := TStyledTestUtils.PaintWinControl(LButton);
    try
      //the face is a pill: it reaches the top and the sides of the control
      TStyledTestUtils.AssertPixel(LBitmap, 100, 30, $FDF9FA, 'face = container of the elevated button');
      LBelow := 0;
      LAbove := 0;
      LLeft := 0;
      for Y := 40 to 59 do
        if IsShadow(TStyledTestUtils.PixelAt(LBitmap, 100, Y)) then
          Inc(LBelow);
      for Y := 0 to 39 do
        if IsShadow(TStyledTestUtils.PixelAt(LBitmap, 100, Y)) then
          Inc(LAbove);
      for X := 0 to 14 do
        if IsShadow(TStyledTestUtils.PixelAt(LBitmap, X, 20)) then
          Inc(LLeft);
      Assert.IsTrue(LBelow >= 2, Format('shadow rows below the face: %d', [LBelow]));
      Assert.AreEqual(0, LAbove, 'the shadow is seen only below: nothing above the face');
      Assert.AreEqual(0, LLeft, 'the shadow is seen only below: nothing at the sides');
      //the last rows of the control are the shadow
      Assert.IsTrue(IsShadow(TStyledTestUtils.PixelAt(LBitmap, 100, 59)), 'last row: shadow');
    finally
      LBitmap.Free;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TAngularM3Tests.Filled_FaceReachesTheEdgeOfTheControl;
var
  LForm: TForm;
  LButton: TStyledButton;
  LBitmap: TBitmap;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LButton := TStyledButton.CreateStyled(LForm, ANGULAR_M3_LIGHT_FAMILY, btn_Basic, FilledAttr);
    LButton.Parent := LForm;
    LButton.SetBounds(0, 0, 200, 60);
    LBitmap := TStyledTestUtils.PaintWinControl(LButton);
    try
      TStyledTestUtils.AssertPixel(LBitmap, 100, 56, $BB5C00, 'Filled face down to the bottom edge');
      TStyledTestUtils.AssertPixel(LBitmap, 100, 3, $BB5C00, 'and up to the top edge');
    finally
      LBitmap.Free;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TAngularM3Tests.Classes_IsOnlyBasic;
var
  LClasses: TButtonClasses;
begin
  //Basic/Link/Disabled of the documentation page are one button (button, anchor,
  //disabled): there is a single class
  LClasses := GetButtonFamilyClasses(ANGULAR_M3_LIGHT_FAMILY);
  Assert.AreEqual(1, Integer(Length(LClasses)), 'number of classes (light)');
  Assert.AreEqual(btn_Basic, LClasses[0]);
  Assert.AreEqual(1, Integer(Length(GetButtonFamilyClasses(ANGULAR_M3_DARK_FAMILY))), 'number of classes (dark)');
end;

procedure TAngularM3Tests.Button_DefaultsToRoundedAndPaintsAPill;
var
  LForm: TForm;
  LButton: TStyledButton;
  LBitmap: TBitmap;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LButton := TStyledButton.CreateStyled(LForm, ANGULAR_M3_LIGHT_FAMILY, btn_Basic, FilledAttr);
    LButton.Parent := LForm;
    LButton.SetBounds(0, 0, 200, 60);
    Assert.AreEqual(Ord(btRounded), Ord(LButton.StyleDrawType), 'default shape');
    LBitmap := TStyledTestUtils.PaintWinControl(LButton);
    try
      TStyledTestUtils.AssertPixel(LBitmap, 100, 12, $BB5C00, 'Filled body = primary');
      TStyledTestUtils.AssertPixel(LBitmap, 6, 6, clPureWhite, 'outside the pill corner');
    finally
      LBitmap.Free;
    end;
  finally
    LForm.Free;
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

{ TStyleBorderWidthTests }

//number of pixels from the left edge that are not the colour of the face, in the middle row
function BorderPixels(const AButton: TStyledButton): Integer;
var
  LBitmap: TBitmap;
  LFace: TColor;
begin
  LBitmap := TStyledTestUtils.PaintWinControl(AButton);
  try
    LFace := TStyledTestUtils.PixelAt(LBitmap, AButton.Width div 2, AButton.Height div 2);
    Result := 0;
    while (Result < 20) and not TStyledTestUtils.SameColor(TStyledTestUtils.PixelAt(LBitmap, Result, AButton.Height div 2), LFace, 2) do
      Inc(Result);
  finally
    LBitmap.Free;
  end;
end;

procedure TStyleBorderWidthTests.Default_IsTheStyleAndNotStored;
var
  LForm: TForm;
  LButton: TStyledButton;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LButton := TStyledButton.CreateStyled(LForm, DEFAULT_CLASSIC_FAMILY, DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE);
    LButton.Parent := LForm;
    Assert.AreEqual(-1, LButton.StyleBorderWidth);
    Assert.AreEqual(-1, DEFAULT_BORDER_WIDTH);
    Assert.IsTrue(Pos('StyleBorderWidth', TStyledTestUtils.DfmText(LButton)) = 0, 'the default is not streamed');
  finally
    LForm.Free;
  end;
end;

procedure TStyleBorderWidthTests.CustomWidth_ReplacesTheBorderOfTheStyleInEveryState;
var
  LForm: TForm;
  LButton: TStyledButton;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LButton := TStyledButton.CreateStyled(LForm, DEFAULT_CLASSIC_FAMILY, DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE);
    LButton.Parent := LForm;
    LButton.StyleDrawType := btRect;
    LButton.SetBounds(0, 0, 100, 40);
    LButton.StyleBorderWidth := 1;
    Assert.AreEqual(1, BorderPixels(LButton), 'width 1');
    LButton.StyleBorderWidth := 4;
    Assert.AreEqual(4, BorderPixels(LButton), 'width 4, normal');
    LButton.Enabled := False;
    Assert.AreEqual(4, BorderPixels(LButton), 'width 4, disabled');
  finally
    LForm.Free;
  end;
end;

procedure TStyleBorderWidthTests.Zero_RemovesTheBorder;
var
  LForm: TForm;
  LButton: TStyledButton;
  LRaised: Boolean;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LButton := TStyledButton.CreateStyled(LForm, DEFAULT_CLASSIC_FAMILY, DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE);
    LButton.Parent := LForm;
    LButton.StyleDrawType := btRect;
    LButton.SetBounds(0, 0, 100, 40);
    LButton.StyleBorderWidth := 0;
    Assert.AreEqual(0, BorderPixels(LButton), 'no border pixels');
    LRaised := False;
    try
      LButton.StyleBorderWidth := -2;
    except
      on EReadError do LRaised := True;
    end;
    Assert.IsTrue(LRaised, 'values below -1 are refused like StyleShadowSize');
  finally
    LForm.Free;
  end;
end;

procedure TStyleBorderWidthTests.CustomWidth_StreamsAndAssigns;
var
  LForm: TForm;
  LButton, LCopy, LOther: TStyledButton;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LButton := TStyledButton.CreateStyled(LForm, DEFAULT_CLASSIC_FAMILY, DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE);
    LButton.Parent := LForm;
    LButton.StyleBorderWidth := 3;
    Assert.IsTrue(Pos('StyleBorderWidth = 3', TStyledTestUtils.DfmText(LButton)) > 0, 'stored when not default');
    LCopy := TStyledTestUtils.StreamRoundTrip(LButton, TStyledButton, LForm) as TStyledButton;
    Assert.AreEqual(3, LCopy.StyleBorderWidth, 'survives streaming');
    LOther := TStyledButton.Create(LForm);
    LOther.Assign(LButton);
    Assert.AreEqual(3, LOther.StyleBorderWidth, 'copied by Assign');
  finally
    LForm.Free;
  end;
end;

procedure TStyleBorderWidthTests.RegisteredDefault_AppliesToNewButtons;
var
  LForm: TForm;
  LButton, LOwn: TStyledButton;
  LRaised: Boolean;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    TStyledButton.RegisterDefaultRenderingStyle(btRect, DEFAULT_CLASSIC_FAMILY,
      DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE, DEFAULT_RADIUS, 5, DEFAULT_CURSOR);
    try
      LButton := TStyledButton.CreateStyled(LForm, DEFAULT_CLASSIC_FAMILY, DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE);
      LButton.Parent := LForm;
      LButton.StyleDrawType := btRect;
      LButton.SetBounds(0, 0, 100, 40);
      Assert.AreEqual(5, LButton.StyleBorderWidth, 'new buttons take the registered default');
      Assert.AreEqual(5, BorderPixels(LButton), 'and paint it');
      LOwn := TStyledButton.CreateStyled(LForm, DEFAULT_CLASSIC_FAMILY, DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE);
      LOwn.StyleBorderWidth := 2;
      Assert.AreEqual(2, LOwn.StyleBorderWidth, 'the value of the button wins');
      LRaised := False;
      try
        TStyledButton.RegisterDefaultRenderingStyle(btRect, DEFAULT_CLASSIC_FAMILY,
          DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE, DEFAULT_RADIUS, -2, DEFAULT_CURSOR);
      except
        on EArgumentException do LRaised := True;
      end;
      Assert.IsTrue(LRaised, 'values below -1 are refused');
    finally
      TStyledButton.RegisterDefaultRenderingStyle(DEFAULT_STYLEDRAWTYPE, DEFAULT_CLASSIC_FAMILY,
        DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE, DEFAULT_RADIUS, DEFAULT_BORDER_WIDTH, DEFAULT_CURSOR);
    end;
    Assert.AreEqual(-1, TStyledButton.CreateStyled(LForm, DEFAULT_CLASSIC_FAMILY, DEFAULT_WINDOWS_CLASS, DEFAULT_APPEARANCE).StyleBorderWidth, 'restored');
  finally
    LForm.Free;
  end;
end;

{ TTransparentCaptionTests }

//The pixel of the caption farthest from the parent colour (sum of the RGB distances)
//and the darkest one (sum of RGB), of a Transparent windowed button on a parent of that colour
procedure CaptionExtremes(const AFamily, AClass, AAppearance: string;
  const AParentColor: TColor; const AEnabled: Boolean;
  out AFarthest, ADarkest: Integer; out ADarkestColor: TColor);
var
  LForm: TForm;
  LButton: TStyledButton;
  LBitmap: Vcl.Graphics.TBitmap;
  LBack, LColor: TColor;
  X, Y, LDistance, LSum: Integer;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LForm.Color := AParentColor;
    LButton := TStyledButton.CreateStyled(LForm, AFamily, AClass, AAppearance);
    LButton.Parent := LForm;
    LButton.StyleDrawType := btRect;
    LButton.Render.Transparent := True;
    LButton.Caption := 'MMMMMMMM';
    LButton.SetBounds(0, 0, 140, 40);
    LButton.Enabled := AEnabled;
    LBitmap := TStyledTestUtils.PaintWinControl(LButton, AParentColor);
    try
      LBack := ColorToRGB(AParentColor);
      AFarthest := 0;
      ADarkest := 3 * 255 + 1;
      ADarkestColor := LBack;
      //inside the border: only the caption can be there
      for Y := 6 to LBitmap.Height - 7 do
        for X := 6 to LBitmap.Width - 7 do
        begin
          LColor := ColorToRGB(TStyledTestUtils.PixelAt(LBitmap, X, Y));
          LDistance := Abs((LColor and $FF) - (LBack and $FF)) +
            Abs((LColor shr 8 and $FF) - (LBack shr 8 and $FF)) +
            Abs((LColor shr 16 and $FF) - (LBack shr 16 and $FF));
          if LDistance > AFarthest then
            AFarthest := LDistance;
          LSum := (LColor and $FF) + (LColor shr 8 and $FF) + (LColor shr 16 and $FF);
          if LSum < ADarkest then
          begin
            ADarkest := LSum;
            ADarkestColor := LColor;
          end;
        end;
    finally
      LBitmap.Free;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TTransparentCaptionTests.Caption_ContrastsWithTheParent(
  const AFamily, AClass, AAppearance: string; const AParentColor: Integer);
var
  LFarthest, LDarkest: Integer;
  LDarkestColor: TColor;
begin
  CaptionExtremes(AFamily, AClass, AAppearance, TColor(AParentColor), True,
    LFarthest, LDarkest, LDarkestColor);
  Assert.IsTrue(LFarthest >= 300, Format('the caption is readable on the parent (distance %d)', [LFarthest]));
end;

procedure TTransparentCaptionTests.ReadableFont_IsKept;
var
  LFarthest, LDarkest: Integer;
  LDarkestColor: TColor;
begin
  //Outline: the font is the blue of the button, which already contrasts with a white parent
  CaptionExtremes(BOOTSTRAP_FAMILY, btn_primary, BOOTSTRAP_OUTLINE, clWhite, True,
    LFarthest, LDarkest, LDarkestColor);
  Assert.IsTrue(LFarthest >= 300, 'the caption is readable');
  Assert.IsTrue((LDarkestColor shr 16 and $FF) > (LDarkestColor and $FF) + 100,
    'the caption keeps the blue of the style (it is not replaced by the text colour)');
end;

procedure TTransparentCaptionTests.DisabledCaption_StaysFaded(
  const AFamily, AClass, AAppearance: string; const AParentColor: Integer);
var
  LFarthestEnabled, LDarkestEnabled, LFarthestDisabled, LDarkestDisabled: Integer;
  LColorEnabled, LColorDisabled: TColor;
begin
  CaptionExtremes(AFamily, AClass, AAppearance, TColor(AParentColor), True,
    LFarthestEnabled, LDarkestEnabled, LColorEnabled);
  CaptionExtremes(AFamily, AClass, AAppearance, TColor(AParentColor), False,
    LFarthestDisabled, LDarkestDisabled, LColorDisabled);
  Assert.IsTrue(LFarthestDisabled < LFarthestEnabled,
    Format('the disabled caption is fainter than the enabled one (%d, enabled %d)',
      [LFarthestDisabled, LFarthestEnabled]));
end;

initialization
  TDUnitX.RegisterTestFixture(TShapeDrawingTests);
  TDUnitX.RegisterTestFixture(TBadgeDrawingTests);
  TDUnitX.RegisterTestFixture(TAngularM3Tests);
  TDUnitX.RegisterTestFixture(TStyleBorderWidthTests);
  TDUnitX.RegisterTestFixture(TTransparentCaptionTests);
  TDUnitX.RegisterTestFixture(TStyleFamilyRegistryTests);
  TDUnitX.RegisterTestFixture(TStyleFamilyContractTests);

end.
