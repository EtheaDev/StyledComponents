{******************************************************************************}
{                                                                              }
{  StyledComponents test suite: shared helpers                                 }
{                                                                              }
{  Copyright (c) 2022-2026 (Ethea S.r.l.)                                      }
{  Author: Carlo Barazzetta                                                    }
{                                                                              }
{  https://github.com/EtheaDev/StyledComponents                                }
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}

/// <summary>
///  Helpers shared by the test suite: painting a control into a 32-bit bitmap
///  (with or without a window), pixel probes that turn "it looks right" into
///  an assertion, DFM streaming round-trips, and the [AllFamilies] attribute
///  that runs one test per registered StyleFamily.
/// </summary>
unit StyledTestUtils;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Rtti,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  DUnitX.TestFramework,
  Vcl.ButtonStylesAttributes;

const
  clPureRed    = TColor($0000FF);
  clPureGreen  = TColor($00FF00);
  clPureBlue   = TColor($FF0000);
  clPureWhite  = TColor($FFFFFF);
  clPureBlack  = TColor($000000);
  clPureYellow = TColor($00FFFF);

type
  /// <summary>
  ///  Runs the decorated test once per registered StyleFamily: the test method
  ///  takes the family name as its only argument.
  /// </summary>
  AllFamiliesAttribute = class(CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray: TestCaseInfoArray; override;
  end;

  TStyledTestUtils = class
  public
    /// <summary>The names of every registered StyleFamily, registration order.</summary>
    class function FamilyNames: TArray<string>; static;
    /// <summary>Five fresh attribute objects (Normal, Pressed, Selected, Hot, Disabled).</summary>
    class procedure NewAttributes(out ANormal, APressed, ASelected, AHot,
      ADisabled: TStyledButtonAttributes); static;
    /// <summary>Frees the five objects and nils them.</summary>
    class procedure FreeAttributes(var ANormal, APressed, ASelected, AHot,
      ADisabled: TStyledButtonAttributes); static;

    /// <summary>A 32-bit bitmap of AWidth x AHeight filled with ABackground.</summary>
    class function NewBitmap(const AWidth, AHeight: Integer;
      const ABackground: TColor = clPureWhite): TBitmap; static;
    /// <summary>
    ///  Paints a TGraphicControl into a new bitmap the size of the control, with
    ///  no parent needed: WM_PAINT with the bitmap DC as wParam draws there.
    ///  The caller frees the bitmap.
    /// </summary>
    class function PaintGraphicControl(const AControl: TGraphicControl;
      const ABackground: TColor = clPureWhite): TBitmap; static;
    /// <summary>
    ///  Paints a TWinControl (which must already have a Parent, see HostForm)
    ///  into a new bitmap the size of the control via PaintTo. The caller frees
    ///  the bitmap.
    /// </summary>
    class function PaintWinControl(const AControl: TWinControl;
      const ABackground: TColor = clPureWhite): TBitmap; static;
    /// <summary>A hidden form to parent windowed controls. The caller frees it.</summary>
    class function HostForm: TForm; static;

    /// <summary>The RGB colour of pixel (X, Y), alpha dropped.</summary>
    class function PixelAt(const ABitmap: TBitmap; const X, Y: Integer): TColor; static;
    /// <summary>
    ///  True when every channel of AActual is within ATolerance of AExpected;
    ///  anti-aliasing is allowed that much slack.
    /// </summary>
    class function SameColor(const AActual, AExpected: TColor;
      const ATolerance: Byte = 40): Boolean; static;
    /// <summary>Colour as #RRGGBB, for the failure messages.</summary>
    class function ColorText(const AColor: TColor): string; static;
    /// <summary>Fails the running test unless pixel (X, Y) of ABitmap is AExpected.</summary>
    class procedure AssertPixel(const ABitmap: TBitmap; const X, Y: Integer;
      const AExpected: TColor; const AWhat: string); static;
    /// <summary>Fails the running test if pixel (X, Y) of ABitmap is AUnexpected.</summary>
    class procedure AssertPixelNot(const ABitmap: TBitmap; const X, Y: Integer;
      const AUnexpected: TColor; const AWhat: string); static;

    /// <summary>
    ///  Streams ASource with WriteComponent and reads it back into a new
    ///  instance of ADestClass with ReadComponent (no RegisterClass needed when
    ///  the instance is passed). The caller frees the result. This is the test
    ///  that catches wrong default/stored specifiers.
    /// </summary>
    class function StreamRoundTrip(const ASource: TComponent;
      const ADestClass: TComponentClass; const AOwner: TComponent = nil): TComponent; static;
    /// <summary>The DFM text of AComponent (ObjectBinaryToText), for diagnostics.</summary>
    class function DfmText(const AComponent: TComponent): string; static;

    /// <summary>A file name in the temporary folder, unique to this run.</summary>
    class function TempFileName(const AExtension: string): string; static;
  end;

implementation

uses
  System.Contnrs;

{ AllFamiliesAttribute }

function AllFamiliesAttribute.GetCaseInfoArray: TestCaseInfoArray;
var
  LNames: TArray<string>;
  I: Integer;
begin
  LNames := TStyledTestUtils.FamilyNames;
  SetLength(Result, Length(LNames));
  for I := 0 to High(LNames) do
  begin
    Result[I].Name := LNames[I];
    SetLength(Result[I].Values, 1);
    Result[I].Values[0] := TValue.From<string>(LNames[I]);
  end;
end;

{ TStyledTestUtils }

class function TStyledTestUtils.FamilyNames: TArray<string>;
var
  LFamilies: TObjectList;
  I: Integer;
begin
  LFamilies := GetButtonFamilies;
  SetLength(Result, LFamilies.Count);
  for I := 0 to LFamilies.Count - 1 do
    Result[I] := GetButtonFamilyName(I);
end;

class procedure TStyledTestUtils.NewAttributes(out ANormal, APressed, ASelected,
  AHot, ADisabled: TStyledButtonAttributes);
begin
  ANormal := TStyledButtonAttributes.Create(nil);
  APressed := TStyledButtonAttributes.Create(nil);
  ASelected := TStyledButtonAttributes.Create(nil);
  AHot := TStyledButtonAttributes.Create(nil);
  ADisabled := TStyledButtonAttributes.Create(nil);
end;

class procedure TStyledTestUtils.FreeAttributes(var ANormal, APressed, ASelected,
  AHot, ADisabled: TStyledButtonAttributes);
begin
  FreeAndNil(ANormal);
  FreeAndNil(APressed);
  FreeAndNil(ASelected);
  FreeAndNil(AHot);
  FreeAndNil(ADisabled);
end;

class function TStyledTestUtils.NewBitmap(const AWidth, AHeight: Integer;
  const ABackground: TColor): TBitmap;
begin
  Result := TBitmap.Create;
  try
    Result.PixelFormat := pf32bit;
    Result.SetSize(AWidth, AHeight);
    Result.Canvas.Brush.Color := ABackground;
    Result.Canvas.FillRect(Rect(0, 0, AWidth, AHeight));
  except
    Result.Free;
    raise;
  end;
end;

class function TStyledTestUtils.PaintGraphicControl(
  const AControl: TGraphicControl; const ABackground: TColor): TBitmap;
begin
  Result := NewBitmap(AControl.Width, AControl.Height, ABackground);
  try
    AControl.Perform(WM_PAINT, WPARAM(Result.Canvas.Handle), 0);
  except
    Result.Free;
    raise;
  end;
end;

class function TStyledTestUtils.PaintWinControl(const AControl: TWinControl;
  const ABackground: TColor): TBitmap;
begin
  Assert.IsNotNull(AControl.Parent, 'PaintWinControl needs a parented control (see HostForm)');
  AControl.HandleNeeded;
  Result := NewBitmap(AControl.Width, AControl.Height, ABackground);
  try
    AControl.PaintTo(Result.Canvas.Handle, 0, 0);
  except
    Result.Free;
    raise;
  end;
end;

class function TStyledTestUtils.HostForm: TForm;
begin
  Result := TForm.CreateNew(nil);
  Result.Visible := False;
  Result.Width := 400;
  Result.Height := 300;
end;

class function TStyledTestUtils.PixelAt(const ABitmap: TBitmap; const X,
  Y: Integer): TColor;
var
  LRow: PRGBQuad;
begin
  Assert.IsTrue((X >= 0) and (X < ABitmap.Width) and (Y >= 0) and (Y < ABitmap.Height),
    Format('Probe (%d,%d) outside a %dx%d bitmap', [X, Y, ABitmap.Width, ABitmap.Height]));
  LRow := ABitmap.ScanLine[Y];
  Inc(LRow, X);
  Result := RGB(LRow.rgbRed, LRow.rgbGreen, LRow.rgbBlue);
end;

class function TStyledTestUtils.SameColor(const AActual, AExpected: TColor;
  const ATolerance: Byte): Boolean;
begin
  Result :=
    (Abs(GetRValue(AActual) - GetRValue(AExpected)) <= ATolerance) and
    (Abs(GetGValue(AActual) - GetGValue(AExpected)) <= ATolerance) and
    (Abs(GetBValue(AActual) - GetBValue(AExpected)) <= ATolerance);
end;

class function TStyledTestUtils.ColorText(const AColor: TColor): string;
begin
  Result := Format('#%.2x%.2x%.2x',
    [GetRValue(AColor), GetGValue(AColor), GetBValue(AColor)]);
end;

class procedure TStyledTestUtils.AssertPixel(const ABitmap: TBitmap; const X,
  Y: Integer; const AExpected: TColor; const AWhat: string);
var
  LActual: TColor;
begin
  LActual := PixelAt(ABitmap, X, Y);
  Assert.IsTrue(SameColor(LActual, AExpected),
    Format('%s: pixel (%d,%d) is %s, expected %s',
      [AWhat, X, Y, ColorText(LActual), ColorText(AExpected)]));
end;

class procedure TStyledTestUtils.AssertPixelNot(const ABitmap: TBitmap; const X,
  Y: Integer; const AUnexpected: TColor; const AWhat: string);
var
  LActual: TColor;
begin
  LActual := PixelAt(ABitmap, X, Y);
  Assert.IsFalse(SameColor(LActual, AUnexpected),
    Format('%s: pixel (%d,%d) is %s, which it must not be',
      [AWhat, X, Y, ColorText(LActual)]));
end;

class function TStyledTestUtils.StreamRoundTrip(const ASource: TComponent;
  const ADestClass: TComponentClass; const AOwner: TComponent): TComponent;
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.WriteComponent(ASource);
    LStream.Position := 0;
    Result := ADestClass.Create(AOwner);
    try
      LStream.ReadComponent(Result);
    except
      Result.Free;
      raise;
    end;
  finally
    LStream.Free;
  end;
end;

class function TStyledTestUtils.DfmText(const AComponent: TComponent): string;
var
  LBinary, LText: TMemoryStream;
  LString: TStringStream;
begin
  LBinary := TMemoryStream.Create;
  LText := TMemoryStream.Create;
  LString := TStringStream.Create('', TEncoding.UTF8);
  try
    LBinary.WriteComponent(AComponent);
    LBinary.Position := 0;
    ObjectBinaryToText(LBinary, LText);
    LText.Position := 0;
    LString.CopyFrom(LText, 0);
    Result := LString.DataString;
  finally
    LString.Free;
    LText.Free;
    LBinary.Free;
  end;
end;

class function TStyledTestUtils.TempFileName(const AExtension: string): string;
var
  LGuid: TGUID;
begin
  CreateGUID(LGuid);
  Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
    'StyledComponentsTests_' + GUIDToString(LGuid).Replace('{', '').Replace('}', '') +
    AExtension;
end;

end.
