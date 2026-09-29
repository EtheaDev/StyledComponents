/// <summary>
///  Performance benchmarks. Each test paints or resolves styles many times,
///  measures wall time and heap allocations, reports the numbers in the test
///  message and fails only above a generous ceiling: the ceilings lock the
///  gains of the 4.3.0 optimisations against regressions, the numbers give
///  the trend (see the NUnit XML report).
///  Allocations are counted by a pass-through memory manager installed at unit
///  initialisation (GetMem/AllocMem/ReallocMem increment a counter).
/// </summary>
unit StyledPerfTests;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  System.Types,
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  Vcl.Graphics,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ButtonGroup,
  Vcl.CategoryButtons,
  DUnitX.TestFramework,
  Vcl.ButtonStylesAttributes,
  Vcl.StandardButtonStyles,
  Vcl.BootstrapButtonStyles,
  Vcl.ColorButtonStyles,
  Vcl.StyledButton,
  Vcl.StyledButtonGroup,
  Vcl.StyledCategoryButtons,
  StyledTestUtils;

type
  TPerfSample = record
    Iterations: Integer;
    Allocations: Int64;
    Microseconds: Int64;
    function AllocsPerIteration: Double;
    function MicrosPerIteration: Double;
    function Text(const AWhat: string): string;
  end;

  [TestFixture]
  TStyledPerfTests = class
  private
    function Measure(const AIterations: Integer; const AProc: TProc): TPerfSample;
  public
    /// <summary>
    ///  P1. Classic (VCL style) buttons re-resolved their style on every paint:
    ///  parent walk, linear scan of ~117 style names, 5 temporary attribute
    ///  objects. 20 buttons x 25 paints.
    /// </summary>
    [Test]
    procedure ClassicButtons_Repaint;

    /// <summary>Guard: a Bootstrap button paint does not resolve anything.</summary>
    [Test]
    procedure BootstrapButtons_Repaint;

    /// <summary>
    ///  P2. ButtonGroup items with their own style swapped the container
    ///  attributes twice per item per paint (2 resolutions, ~10 allocations).
    ///  40 custom items x 20 paints.
    /// </summary>
    [Test]
    procedure ButtonGroup_40CustomItems_Repaint;

    /// <summary>P2. Same for CategoryButtons.</summary>
    [Test]
    procedure CategoryButtons_40CustomItems_Repaint;

    /// <summary>P3. Classic style lookup by name (linear scan) x 10000.</summary>
    [Test]
    procedure Classic_StyleLookup;

    /// <summary>P3. SVG-Colors unknown class resolved through EConvertError x 500.</summary>
    [Test]
    procedure SvgColors_UnknownClass_Resolve;

    // ---- breakdown of a button paint (reference numbers, no ceiling) ----

    /// <summary>Reference: a plain VCL TButton painted the same way.</summary>
    [Test]
    procedure Reference_VclButton_Repaint;
    /// <summary>GDI+ shape only: CanvasDrawShape btRoundRect 120x30.</summary>
    [Test]
    procedure Breakdown_Shape_RoundRect;
    /// <summary>GDI+ text only: CanvasDrawText.</summary>
    [Test]
    procedure Breakdown_Text;
    /// <summary>Style resolution only: StyleFamilyUpdateAttributes Classic/Windows.</summary>
    [Test]
    procedure Breakdown_StyleResolve_Classic;
    /// <summary>Active style name only: GetActiveStyleName(control).</summary>
    [Test]
    procedure Breakdown_ActiveStyleName;
    /// <summary>Cost of a 120x30 TBitmap create/free (the per-paint buffer).</summary>
    [Test]
    procedure Breakdown_BufferBitmap;
    /// <summary>Cost of a TGPGraphics create/free on a DC (one per GDI+ helper call).</summary>
    [Test]
    procedure Breakdown_GdiPlusGraphics;
    /// <summary>Plain GDI DrawText for comparison with the GDI+ text path.</summary>
    [Test]
    procedure Breakdown_GdiDrawText;

    // ---- real WM_PAINT path (visible off-screen form, InvalidateRect + UpdateWindow) ----

    /// <summary>
    ///  O1. The double-buffered WM_PAINT created and freed the paint TBitmap on
    ///  every paint (the buffer was released as soon as the paint ended).
    ///  20 Bootstrap buttons x 25 repaints through the message loop.
    /// </summary>
    [Test]
    procedure Buttons_WmPaint_DoubleBuffered;
    /// <summary>Same buttons, DoubleBuffered off (reference for the buffer cost).</summary>
    [Test]
    procedure Buttons_WmPaint_NotDoubleBuffered;
    /// <summary>Reference: VCL TButton through the same message loop.</summary>
    [Test]
    procedure Reference_VclButton_WmPaint;
    /// <summary>P4. TStyledBitBtn Kind=bkOK: glyph decoded from resource on every paint.</summary>
    [Test]
    procedure BitBtn_KindOk_Repaint;
    /// <summary>P4. Command link button: icon decoded from resource on every paint.</summary>
    [Test]
    procedure CommandLink_Repaint;
    /// <summary>P5. TStyledBitBtn with a custom 2-state glyph: TImageList built on every paint.</summary>
    [Test]
    procedure BitBtn_CustomGlyph_Repaint;
  end;

/// <summary>Heap allocations counted so far by the pass-through memory manager.</summary>
function PerfAllocationCount: Int64;

implementation

uses
  System.Math;

var
  GOldMM: TMemoryManagerEx;
  GAllocCount: Int64;
  GCounterInstalled: Boolean;

function CountingGetMem(Size: NativeInt): Pointer;
begin
  AtomicIncrement(GAllocCount);
  Result := GOldMM.GetMem(Size);
end;

function CountingFreeMem(P: Pointer): Integer;
begin
  Result := GOldMM.FreeMem(P);
end;

function CountingReallocMem(P: Pointer; Size: NativeInt): Pointer;
begin
  AtomicIncrement(GAllocCount);
  Result := GOldMM.ReallocMem(P, Size);
end;

function CountingAllocMem(Size: NativeInt): Pointer;
begin
  AtomicIncrement(GAllocCount);
  Result := GOldMM.AllocMem(Size);
end;

function CountingRegisterLeak(P: Pointer): Boolean;
begin
  Result := GOldMM.RegisterExpectedMemoryLeak(P);
end;

function CountingUnregisterLeak(P: Pointer): Boolean;
begin
  Result := GOldMM.UnregisterExpectedMemoryLeak(P);
end;

procedure InstallCounter;
var
  LNew: TMemoryManagerEx;
begin
  GetMemoryManager(GOldMM);
  LNew.GetMem := CountingGetMem;
  LNew.FreeMem := CountingFreeMem;
  LNew.ReallocMem := CountingReallocMem;
  LNew.AllocMem := CountingAllocMem;
  LNew.RegisterExpectedMemoryLeak := CountingRegisterLeak;
  LNew.UnregisterExpectedMemoryLeak := CountingUnregisterLeak;
  SetMemoryManager(LNew);
  GCounterInstalled := True;
end;

function PerfAllocationCount: Int64;
begin
  Result := GAllocCount;
end;

{ TPerfSample }

function TPerfSample.AllocsPerIteration: Double;
begin
  Result := Allocations / Max(1, Iterations);
end;

function TPerfSample.MicrosPerIteration: Double;
begin
  Result := Microseconds / Max(1, Iterations);
end;

function TPerfSample.Text(const AWhat: string): string;
begin
  Result := Format('%s: %d iterations, %.1f allocations and %.1f us per iteration (total %d allocations, %d ms)',
    [AWhat, Iterations, AllocsPerIteration, MicrosPerIteration, Allocations, Microseconds div 1000]);
end;

{ TStyledPerfTests }

function TStyledPerfTests.Measure(const AIterations: Integer; const AProc: TProc): TPerfSample;
var
  LWatch: TStopwatch;
  LAllocs: Int64;
  I: Integer;
begin
  Assert.IsTrue(GCounterInstalled, 'allocation counter installed');
  // warm-up: first paint pays one-off costs (font handles, GDI+ start-up)
  AProc;
  LAllocs := GAllocCount;
  LWatch := TStopwatch.StartNew;
  for I := 1 to AIterations do
    AProc;
  LWatch.Stop;
  Result.Iterations := AIterations;
  Result.Allocations := GAllocCount - LAllocs;
  Result.Microseconds := LWatch.Elapsed.Ticks div 10; // 100 ns ticks
end;

procedure TStyledPerfTests.ClassicButtons_Repaint;
const
  BUTTONS = 20;
  PAINTS = 25;
  MAX_ALLOCS_PER_PAINT = 16; // 4.2.4: 19 (style re-resolved on every paint), 4.3.0: 11
var
  LForm: TForm;
  LButtons: array[0..BUTTONS - 1] of TStyledButton;
  LBitmap: TBitmap;
  I: Integer;
  LSample: TPerfSample;
begin
  LForm := TStyledTestUtils.HostForm;
  LBitmap := TStyledTestUtils.NewBitmap(120, 30, clWhite);
  try
    for I := 0 to BUTTONS - 1 do
    begin
      LButtons[I] := TStyledButton.Create(LForm); // Classic / Windows by default
      LButtons[I].Parent := LForm;
      LButtons[I].SetBounds(10 + (I mod 3) * 125, 10 + (I div 3) * 35, 120, 30);
      LButtons[I].Caption := 'Button ' + IntToStr(I);
      LButtons[I].HandleNeeded;
    end;
    LSample := Measure(PAINTS, procedure
      var
        J: Integer;
      begin
        for J := 0 to BUTTONS - 1 do
          LButtons[J].PaintTo(LBitmap.Canvas.Handle, 0, 0);
      end);
    LSample.Iterations := PAINTS * BUTTONS; // per single button paint
    Assert.IsTrue(LSample.AllocsPerIteration <= MAX_ALLOCS_PER_PAINT,
      LSample.Text('Classic button paint') + Format(' - ceiling %d allocations per paint', [MAX_ALLOCS_PER_PAINT]));
    Assert.Pass(LSample.Text('Classic button paint'));
  finally
    LBitmap.Free;
    LForm.Free;
  end;
end;

procedure TStyledPerfTests.BootstrapButtons_Repaint;
const
  BUTTONS = 20;
  PAINTS = 25;
  MAX_ALLOCS_PER_PAINT = 16; // 4.2.4 and 4.3.0: 14 (nothing resolved per paint)
var
  LForm: TForm;
  LButtons: array[0..BUTTONS - 1] of TStyledButton;
  LBitmap: TBitmap;
  I: Integer;
  LSample: TPerfSample;
begin
  LForm := TStyledTestUtils.HostForm;
  LBitmap := TStyledTestUtils.NewBitmap(120, 30, clWhite);
  try
    for I := 0 to BUTTONS - 1 do
    begin
      LButtons[I] := TStyledButton.CreateStyled(LForm, BOOTSTRAP_FAMILY, btn_primary, BOOTSTRAP_NORMAL);
      LButtons[I].Parent := LForm;
      LButtons[I].SetBounds(10 + (I mod 3) * 125, 10 + (I div 3) * 35, 120, 30);
      LButtons[I].Caption := 'Button ' + IntToStr(I);
      LButtons[I].HandleNeeded;
    end;
    LSample := Measure(PAINTS, procedure
      var
        J: Integer;
      begin
        for J := 0 to BUTTONS - 1 do
          LButtons[J].PaintTo(LBitmap.Canvas.Handle, 0, 0);
      end);
    LSample.Iterations := PAINTS * BUTTONS;
    Assert.IsTrue(LSample.AllocsPerIteration <= MAX_ALLOCS_PER_PAINT,
      LSample.Text('Bootstrap button paint') + Format(' - ceiling %d allocations per paint', [MAX_ALLOCS_PER_PAINT]));
    Assert.Pass(LSample.Text('Bootstrap button paint'));
  finally
    LBitmap.Free;
    LForm.Free;
  end;
end;

procedure TStyledPerfTests.ButtonGroup_40CustomItems_Repaint;
const
  ITEMS = 40;
  PAINTS = 20;
  MAX_ALLOCS_PER_ITEM_PAINT = 12; // 4.2.4: 27 (attributes resolved twice per item per paint), 4.3.0: 9
  CLASSES: array[0..2] of string = (btn_danger, btn_success, btn_warning);
var
  LForm: TForm;
  LGroup: TStyledButtonGroup;
  LItem: TStyledGrpButtonItem;
  LBitmap: TBitmap;
  I: Integer;
  LSample: TPerfSample;
begin
  LForm := TStyledTestUtils.HostForm;
  LForm.Width := 300;
  LForm.Height := 900;
  LGroup := TStyledButtonGroup.CreateStyled(LForm, BOOTSTRAP_FAMILY, btn_primary, BOOTSTRAP_NORMAL);
  LGroup.Parent := LForm;
  LGroup.SetBounds(0, 0, 280, 880);
  LGroup.ButtonHeight := 20;
  LGroup.ButtonOptions := LGroup.ButtonOptions + [gboFullSize, gboShowCaptions];
  for I := 0 to ITEMS - 1 do
  begin
    LItem := LGroup.Items.Add;
    LItem.Caption := 'Item ' + IntToStr(I);
    LItem.StyleClass := CLASSES[I mod 3]; // custom style: differs from the container
  end;
  LGroup.HandleNeeded;
  LBitmap := TStyledTestUtils.NewBitmap(LGroup.Width, LGroup.Height, clWhite);
  try
    LSample := Measure(PAINTS, procedure
      begin
        LGroup.PaintTo(LBitmap.Canvas.Handle, 0, 0);
      end);
    LSample.Iterations := PAINTS * ITEMS; // per item paint
    Assert.IsTrue(LSample.AllocsPerIteration <= MAX_ALLOCS_PER_ITEM_PAINT,
      LSample.Text('ButtonGroup custom item paint') + Format(' - ceiling %d allocations per item paint', [MAX_ALLOCS_PER_ITEM_PAINT]));
    Assert.Pass(LSample.Text('ButtonGroup custom item paint'));
  finally
    LBitmap.Free;
    LForm.Free;
  end;
end;

procedure TStyledPerfTests.CategoryButtons_40CustomItems_Repaint;
const
  ITEMS = 40;
  PAINTS = 20;
  MAX_ALLOCS_PER_ITEM_PAINT = 12; // 4.2.4: 27, 4.3.0: 9
  CLASSES: array[0..2] of string = (btn_danger, btn_success, btn_warning);
var
  LForm: TForm;
  LButtons: TStyledCategoryButtons;
  LCategory: TStyledButtonCategory;
  LItem: TStyledButtonItem;
  LBitmap: TBitmap;
  I: Integer;
  LSample: TPerfSample;
begin
  LForm := TStyledTestUtils.HostForm;
  LForm.Width := 300;
  LForm.Height := 1000;
  LButtons := TStyledCategoryButtons.CreateStyled(LForm, BOOTSTRAP_FAMILY, btn_primary, BOOTSTRAP_NORMAL);
  LButtons.Parent := LForm;
  LButtons.SetBounds(0, 0, 280, 980);
  LButtons.ButtonHeight := 20;
  LButtons.ButtonOptions := LButtons.ButtonOptions + [boFullSize, boShowCaptions];
  LCategory := LButtons.Categories.Add;
  LCategory.Caption := 'Category';
  for I := 0 to ITEMS - 1 do
  begin
    LItem := LCategory.Items.Add as TStyledButtonItem;
    LItem.Caption := 'Item ' + IntToStr(I);
    LItem.StyleClass := CLASSES[I mod 3];
  end;
  LButtons.HandleNeeded;
  LBitmap := TStyledTestUtils.NewBitmap(LButtons.Width, LButtons.Height, clWhite);
  try
    LSample := Measure(PAINTS, procedure
      begin
        LButtons.PaintTo(LBitmap.Canvas.Handle, 0, 0);
      end);
    LSample.Iterations := PAINTS * ITEMS;
    Assert.IsTrue(LSample.AllocsPerIteration <= MAX_ALLOCS_PER_ITEM_PAINT,
      LSample.Text('CategoryButtons custom item paint') + Format(' - ceiling %d allocations per item paint', [MAX_ALLOCS_PER_ITEM_PAINT]));
    Assert.Pass(LSample.Text('CategoryButtons custom item paint'));
  finally
    LBitmap.Free;
    LForm.Free;
  end;
end;

procedure TStyledPerfTests.Classic_StyleLookup;
const
  LOOKUPS = 10000;
  MAX_MICROS_PER_LOOKUP = 20.0; // measured ~1-2 us (linear scan of ~117 names)
var
  LSample: TPerfSample;
  LClasses: TButtonClasses;
begin
  LClasses := GetButtonFamilyClasses(DEFAULT_CLASSIC_FAMILY);
  Assert.IsTrue(Length(LClasses) > 10, 'VCL style table registered (manifest)');
  LSample := Measure(LOOKUPS, procedure
    var
      LTheme: TThemeAttribute;
      LClass, LAppearance: string;
      LFamily: TButtonFamily;
    begin
      // the last registered style is the worst case of a linear scan
      GetStyleAttributes(LClasses[High(LClasses)], LTheme);
      LClass := LClasses[High(LClasses)];
      LAppearance := DEFAULT_APPEARANCE;
      StyleFamilyCheckAttributes(DEFAULT_CLASSIC_FAMILY, LClass, LAppearance, LFamily);
    end);
  Assert.IsTrue(LSample.MicrosPerIteration <= MAX_MICROS_PER_LOOKUP,
    LSample.Text('Classic style lookup') + Format(' - ceiling %.1f us', [MAX_MICROS_PER_LOOKUP]));
  Assert.Pass(LSample.Text('Classic style lookup'));
end;

procedure TStyledPerfTests.SvgColors_UnknownClass_Resolve;
const
  RESOLVES = 500;
  MAX_MICROS_PER_RESOLVE = 15.0; // 4.2.4: 36-53 us (EConvertError per resolution), 4.3.0: ~4 us
var
  LSample: TPerfSample;
  N, P, S, H, D: TStyledButtonAttributes;
begin
  TStyledTestUtils.NewAttributes(N, P, S, H, D);
  try
    LSample := Measure(RESOLVES, procedure
      var
        LClass, LAppearance: string;
      begin
        LClass := 'NoSuchColourName';
        LAppearance := COLOR_BTN_NORMAL;
        StyleFamilyUpdateAttributes(SVG_COLOR_FAMILY, LClass, LAppearance, N, P, S, H, D);
      end);
    Assert.IsTrue(LSample.MicrosPerIteration <= MAX_MICROS_PER_RESOLVE,
      LSample.Text('SVG-Colors unknown class') + Format(' - ceiling %.1f us', [MAX_MICROS_PER_RESOLVE]));
    Assert.Pass(LSample.Text('SVG-Colors unknown class'));
  finally
    TStyledTestUtils.FreeAttributes(N, P, S, H, D);
  end;
end;

procedure TStyledPerfTests.Reference_VclButton_Repaint;
const
  BUTTONS = 20;
  PAINTS = 25;
var
  LForm: TForm;
  LButtons: array[0..BUTTONS - 1] of TButton;
  LBitmap: TBitmap;
  I: Integer;
  LSample: TPerfSample;
begin
  LForm := TStyledTestUtils.HostForm;
  LBitmap := TStyledTestUtils.NewBitmap(120, 30, clWhite);
  try
    for I := 0 to BUTTONS - 1 do
    begin
      LButtons[I] := TButton.Create(LForm);
      LButtons[I].Parent := LForm;
      LButtons[I].SetBounds(10 + (I mod 3) * 125, 10 + (I div 3) * 35, 120, 30);
      LButtons[I].Caption := 'Button ' + IntToStr(I);
      LButtons[I].HandleNeeded;
    end;
    LSample := Measure(PAINTS, procedure
      var
        J: Integer;
      begin
        for J := 0 to BUTTONS - 1 do
          LButtons[J].PaintTo(LBitmap.Canvas.Handle, 0, 0);
      end);
    LSample.Iterations := PAINTS * BUTTONS;
    Assert.Pass(LSample.Text('VCL TButton paint (reference)'));
  finally
    LBitmap.Free;
    LForm.Free;
  end;
end;

procedure TStyledPerfTests.Breakdown_Shape_RoundRect;
var
  LBitmap: TBitmap;
  LSample: TPerfSample;
begin
  LBitmap := TStyledTestUtils.NewBitmap(120, 30, clWhite);
  try
    LBitmap.Canvas.Pen.Width := 1;
    LBitmap.Canvas.Pen.Color := clNavy;
    LBitmap.Canvas.Brush.Color := clSkyBlue;
    LSample := Measure(1000, procedure
      begin
        CanvasDrawShape(LBitmap.Canvas, Rect(0, 0, 120, 30), btRoundRect, 6, ALL_ROUNDED_CORNERS);
      end);
    Assert.Pass(LSample.Text('CanvasDrawShape btRoundRect'));
  finally
    LBitmap.Free;
  end;
end;

procedure TStyledPerfTests.Breakdown_Text;
var
  LBitmap: TBitmap;
  LSample: TPerfSample;
begin
  LBitmap := TStyledTestUtils.NewBitmap(120, 30, clWhite);
  try
    LBitmap.Canvas.Font.Name := 'Segoe UI';
    LBitmap.Canvas.Font.Size := 9;
    LSample := Measure(1000, procedure
      begin
        CanvasDrawText(LBitmap.Canvas, Rect(0, 0, 120, 30), 'Button 1', DT_CENTER or DT_VCENTER or DT_SINGLELINE);
      end);
    Assert.Pass(LSample.Text('CanvasDrawText'));
  finally
    LBitmap.Free;
  end;
end;

procedure TStyledPerfTests.Breakdown_StyleResolve_Classic;
var
  N, P, S, H, D: TStyledButtonAttributes;
  LSample: TPerfSample;
begin
  TStyledTestUtils.NewAttributes(N, P, S, H, D);
  try
    LSample := Measure(1000, procedure
      var
        LClass, LAppearance: string;
      begin
        LClass := DEFAULT_WINDOWS_CLASS;
        LAppearance := DEFAULT_APPEARANCE;
        StyleFamilyUpdateAttributes(DEFAULT_CLASSIC_FAMILY, LClass, LAppearance, N, P, S, H, D);
      end);
    Assert.Pass(LSample.Text('StyleFamilyUpdateAttributes Classic/Windows'));
  finally
    TStyledTestUtils.FreeAttributes(N, P, S, H, D);
  end;
end;

procedure TStyledPerfTests.Breakdown_ActiveStyleName;
var
  LForm: TForm;
  LButton: TStyledButton;
  LSample: TPerfSample;
begin
  LForm := TStyledTestUtils.HostForm;
  try
    LButton := TStyledButton.Create(LForm);
    LButton.Parent := LForm;
    LSample := Measure(1000, procedure
      begin
        GetActiveStyleName(LButton);
      end);
    Assert.Pass(LSample.Text('GetActiveStyleName'));
  finally
    LForm.Free;
  end;
end;

procedure TStyledPerfTests.Breakdown_BufferBitmap;
var
  LSample: TPerfSample;
begin
  LSample := Measure(1000, procedure
    var
      LBitmap: TBitmap;
    begin
      LBitmap := TBitmap.Create;
      try
        LBitmap.PixelFormat := pf32bit;
        LBitmap.SetSize(120, 30);
        LBitmap.Canvas.Handle; // force the DC like a paint would
      finally
        LBitmap.Free;
      end;
    end);
  Assert.Pass(LSample.Text('TBitmap 120x30 create/free'));
end;

procedure TStyledPerfTests.Breakdown_GdiPlusGraphics;
var
  LBitmap: TBitmap;
  LSample: TPerfSample;
begin
  LBitmap := TStyledTestUtils.NewBitmap(120, 30, clWhite);
  try
    LSample := Measure(1000, procedure
      var
        LGraphics: TGPGraphics;
      begin
        LGraphics := TGPGraphics.Create(LBitmap.Canvas.Handle);
        LGraphics.SetSmoothingMode(SmoothingModeAntiAlias);
        LGraphics.Free;
      end);
    Assert.Pass(LSample.Text('TGPGraphics create/free'));
  finally
    LBitmap.Free;
  end;
end;

procedure TStyledPerfTests.Breakdown_GdiDrawText;
var
  LBitmap: TBitmap;
  LSample: TPerfSample;
begin
  LBitmap := TStyledTestUtils.NewBitmap(120, 30, clWhite);
  try
    LBitmap.Canvas.Font.Name := 'Segoe UI';
    LBitmap.Canvas.Font.Size := 9;
    LSample := Measure(1000, procedure
      var
        R: TRect;
      begin
        R := Rect(0, 0, 120, 30);
        Winapi.Windows.DrawText(LBitmap.Canvas.Handle, 'Button 1', 8, R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
      end);
    Assert.Pass(LSample.Text('GDI DrawText'));
  finally
    LBitmap.Free;
  end;
end;

function OffScreenForm: TForm;
begin
  Result := TStyledTestUtils.HostForm;
  Result.Position := poDesigned;
  Result.Left := -32000;
  Result.Top := -32000;
end;

procedure RepaintNow(const AControl: TWinControl);
begin
  InvalidateRect(AControl.Handle, nil, False);
  UpdateWindow(AControl.Handle);
end;

procedure TStyledPerfTests.Buttons_WmPaint_DoubleBuffered;
const
  BUTTONS = 20;
  PAINTS = 25;
  MAX_ALLOCS_PER_PAINT = 10; // 4.2.4: 21 (paint TBitmap created and freed per paint), 4.3.0: 7
var
  LForm: TForm;
  LButtons: array[0..BUTTONS - 1] of TStyledButton;
  I: Integer;
  LSample: TPerfSample;
begin
  LForm := OffScreenForm;
  try
    for I := 0 to BUTTONS - 1 do
    begin
      LButtons[I] := TStyledButton.CreateStyled(LForm, BOOTSTRAP_FAMILY, btn_primary, BOOTSTRAP_NORMAL);
      LButtons[I].Parent := LForm;
      LButtons[I].SetBounds(10 + (I mod 3) * 125, 10 + (I div 3) * 35, 120, 30);
      LButtons[I].Caption := 'Button ' + IntToStr(I);
      LButtons[I].DoubleBuffered := True;
    end;
    LForm.Show;
    try
      LSample := Measure(PAINTS, procedure
        var
          J: Integer;
        begin
          for J := 0 to BUTTONS - 1 do
            RepaintNow(LButtons[J]);
        end);
    finally
      LForm.Hide;
    end;
    LSample.Iterations := PAINTS * BUTTONS;
    Assert.IsTrue(LSample.AllocsPerIteration <= MAX_ALLOCS_PER_PAINT,
      LSample.Text('WM_PAINT double-buffered') + Format(' - ceiling %d allocations per paint', [MAX_ALLOCS_PER_PAINT]));
    Assert.Pass(LSample.Text('WM_PAINT double-buffered'));
  finally
    LForm.Free;
  end;
end;

procedure TStyledPerfTests.Buttons_WmPaint_NotDoubleBuffered;
const
  BUTTONS = 20;
  PAINTS = 25;
var
  LForm: TForm;
  LButtons: array[0..BUTTONS - 1] of TStyledButton;
  I: Integer;
  LSample: TPerfSample;
begin
  LForm := OffScreenForm;
  try
    for I := 0 to BUTTONS - 1 do
    begin
      LButtons[I] := TStyledButton.CreateStyled(LForm, BOOTSTRAP_FAMILY, btn_primary, BOOTSTRAP_NORMAL);
      LButtons[I].Parent := LForm;
      LButtons[I].SetBounds(10 + (I mod 3) * 125, 10 + (I div 3) * 35, 120, 30);
      LButtons[I].Caption := 'Button ' + IntToStr(I);
      LButtons[I].DoubleBuffered := False;
    end;
    LForm.Show;
    try
      LSample := Measure(PAINTS, procedure
        var
          J: Integer;
        begin
          for J := 0 to BUTTONS - 1 do
            RepaintNow(LButtons[J]);
        end);
    finally
      LForm.Hide;
    end;
    LSample.Iterations := PAINTS * BUTTONS;
    Assert.Pass(LSample.Text('WM_PAINT not double-buffered'));
  finally
    LForm.Free;
  end;
end;

procedure TStyledPerfTests.Reference_VclButton_WmPaint;
const
  BUTTONS = 20;
  PAINTS = 25;
var
  LForm: TForm;
  LButtons: array[0..BUTTONS - 1] of TButton;
  I: Integer;
  LSample: TPerfSample;
begin
  LForm := OffScreenForm;
  try
    for I := 0 to BUTTONS - 1 do
    begin
      LButtons[I] := TButton.Create(LForm);
      LButtons[I].Parent := LForm;
      LButtons[I].SetBounds(10 + (I mod 3) * 125, 10 + (I div 3) * 35, 120, 30);
      LButtons[I].Caption := 'Button ' + IntToStr(I);
    end;
    LForm.Show;
    try
      LSample := Measure(PAINTS, procedure
        var
          J: Integer;
        begin
          for J := 0 to BUTTONS - 1 do
            RepaintNow(LButtons[J]);
        end);
    finally
      LForm.Hide;
    end;
    LSample.Iterations := PAINTS * BUTTONS;
    Assert.Pass(LSample.Text('VCL TButton WM_PAINT (reference)'));
  finally
    LForm.Free;
  end;
end;

procedure TStyledPerfTests.BitBtn_KindOk_Repaint;
const
  PAINTS = 200;
  MAX_ALLOCS_PER_PAINT = 20; // 4.2.4: 38 (PNG decoded per paint, ~950 us), 4.3.0: 14 (~210 us)
var
  LForm: TForm;
  LButton: TStyledBitBtn;
  LBitmap: TBitmap;
  LSample: TPerfSample;
begin
  LForm := TStyledTestUtils.HostForm;
  LBitmap := TStyledTestUtils.NewBitmap(120, 30, clWhite);
  try
    LButton := TStyledBitBtn.Create(LForm);
    LButton.Parent := LForm;
    LButton.SetBounds(10, 10, 120, 30);
    LButton.Kind := bkOK;
    LButton.HandleNeeded;
    LSample := Measure(PAINTS, procedure
      begin
        LButton.PaintTo(LBitmap.Canvas.Handle, 0, 0);
      end);
    Assert.IsTrue(LSample.AllocsPerIteration <= MAX_ALLOCS_PER_PAINT,
      LSample.Text('TStyledBitBtn bkOK paint') + Format(' - ceiling %d allocations per paint', [MAX_ALLOCS_PER_PAINT]));
    Assert.Pass(LSample.Text('TStyledBitBtn bkOK paint'));
  finally
    LBitmap.Free;
    LForm.Free;
  end;
end;

procedure TStyledPerfTests.CommandLink_Repaint;
const
  PAINTS = 200;
  MAX_ALLOCS_PER_PAINT = 32; // 4.2.4: 49 (PNG decoded per paint, ~660 us), 4.3.0: 25 (~360 us)
var
  LForm: TForm;
  LButton: TStyledButton;
  LBitmap: TBitmap;
  LSample: TPerfSample;
begin
  LForm := TStyledTestUtils.HostForm;
  LBitmap := TStyledTestUtils.NewBitmap(200, 60, clWhite);
  try
    LButton := TStyledButton.Create(LForm);
    LButton.Parent := LForm;
    LButton.SetBounds(10, 10, 200, 60);
    LButton.Style := TCustomButton.TButtonStyle.bsCommandLink;
    LButton.Caption := 'Command link';
    LButton.CommandLinkHint := 'hint text';
    LButton.HandleNeeded;
    LSample := Measure(PAINTS, procedure
      begin
        LButton.PaintTo(LBitmap.Canvas.Handle, 0, 0);
      end);
    Assert.IsTrue(LSample.AllocsPerIteration <= MAX_ALLOCS_PER_PAINT,
      LSample.Text('Command link paint') + Format(' - ceiling %d allocations per paint', [MAX_ALLOCS_PER_PAINT]));
    Assert.Pass(LSample.Text('Command link paint'));
  finally
    LBitmap.Free;
    LForm.Free;
  end;
end;

procedure TStyledPerfTests.BitBtn_CustomGlyph_Repaint;
const
  PAINTS = 200;
  MAX_ALLOCS_PER_PAINT = 20; // 4.2.4: 37 (TImageList built per paint, ~560 us), 4.3.0: see report
var
  LForm: TForm;
  LButton: TStyledBitBtn;
  LGlyph, LBitmap: TBitmap;
  LSample: TPerfSample;
begin
  LForm := TStyledTestUtils.HostForm;
  LBitmap := TStyledTestUtils.NewBitmap(120, 30, clWhite);
  LGlyph := TStyledTestUtils.NewBitmap(32, 16, clOlive); // 2 glyphs of 16x16, olive = transparent
  try
    LGlyph.Canvas.Brush.Color := clRed;
    LGlyph.Canvas.Ellipse(2, 2, 14, 14);
    LGlyph.Canvas.Brush.Color := clGray;
    LGlyph.Canvas.Ellipse(18, 2, 30, 14);
    LButton := TStyledBitBtn.Create(LForm);
    LButton.Parent := LForm;
    LButton.SetBounds(10, 10, 120, 30);
    LButton.Caption := 'Custom';
    LButton.NumGlyphs := 2;
    LButton.Glyph := LGlyph;
    LButton.HandleNeeded;
    LSample := Measure(PAINTS, procedure
      begin
        LButton.PaintTo(LBitmap.Canvas.Handle, 0, 0);
      end);
    Assert.IsTrue(LSample.AllocsPerIteration <= MAX_ALLOCS_PER_PAINT,
      LSample.Text('TStyledBitBtn custom glyph paint') + Format(' - ceiling %d allocations per paint', [MAX_ALLOCS_PER_PAINT]));
    Assert.Pass(LSample.Text('TStyledBitBtn custom glyph paint'));
  finally
    LGlyph.Free;
    LBitmap.Free;
    LForm.Free;
  end;
end;

initialization
  InstallCounter;
  TDUnitX.RegisterTestFixture(TStyledPerfTests);

end.
