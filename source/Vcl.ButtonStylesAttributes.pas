{******************************************************************************}
{                                                                              }
{       StyledButton: a Button Component based on TGraphicControl              }
{                                                                              }
{       Copyright (c) 2022 (Ethea S.r.l.)                                      }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
{                                                                              }
{       https://github.com/EtheaDev/StyledComponents                           }
{                                                                              }
{******************************************************************************}
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
unit Vcl.ButtonStylesAttributes;

interface

{$INCLUDE StyledComponents.inc}

uses
  Vcl.Graphics
  , System.Classes
  , System.Contnrs
  , System.Types
  , Vcl.Controls;

const
  DEFAULT_RADIUS = 6;

resourcestring
  ERROR_FAMILY_NOT_FOUND = 'Styled Button Family "%s" not found';

Type
  //string typed attributes
  TStyledButtonFamily = string;
  TStyledButtonClass = string;
  TStyledButtonAppearance = string;

  //Type of border
  TBtnBorder = (btRounded, btSquare, btRectangle, btCircle);

  //List of available elements
  TButtonFamilies = array of TStyledButtonFamily;
  TButtonClasses = Array of TStyledButtonClass;
  TButtonAppearances = Array of TStyledButtonAppearance;

  TStyledButtonAttributes = class(TComponent)
  private
    FBorderType: TBtnBorder;
    FBorderWidth: Integer;
    FBorderStyle: TPenStyle;
    FBrushStyle: TBrushStyle;
    FBorderColor: TColor;
    FFontColor: TColor;
    FFontStyle: TFontStyles;
    FFontName: TFontName;
    FButtonColor: TColor;
    FOwnerControl: TGraphicControl;
    FIsChanged: Boolean;
    FRadius: Integer;
    procedure InvalidateControl;
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderStyle(const Value: TPenStyle);
    procedure SetBrushStyle(const Value: TBrushStyle);
    procedure SetBorderType(const Value: TBtnBorder);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetButtonColor(const Value: TColor);
    procedure SetFontColor(const Value: TColor);
    procedure SetFontName(const Value: TFontName);
    procedure SetFontStyle(const Value: TFontStyles);
    procedure SetRadius(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetChanged;
    function IsChanged: Boolean;
    procedure Assign(ASource: TPersistent); override;
  published
    property BorderType : TBtnBorder  read FBorderType   write SetBorderType;
    property BorderWidth: Integer     read FBorderWidth  write SetBorderWidth;
    property BorderStyle: TPenStyle   read FBorderStyle  write SetBorderStyle;
    property BrushStyle : TBrushStyle read FBrushStyle   write SetBrushStyle;
    property BorderColor: TColor      read FBorderColor  write SetBorderColor;
    property FontColor  : TColor      read FFontColor    write SetFontColor;
    property FontStyle  : TFontStyles read FFontStyle    write SetFontStyle;
    property FontName   : TFontName   read FFontName     write SetFontName;
    property ButtonColor: TColor      read FButtonColor  write SetButtonColor;
    property Radius     : Integer     read FRadius       write SetRadius;
  end;

  //  Abstraction of Graphic Button Attributes
  IStyledButtonAttributes = interface
    ['{C7BC98EE-D513-46B9-881A-2FDE8DE07786}']
    procedure UpdateAttributes(
      const AFamily:  TStyledButtonFamily;
      const AStyle: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance;
      var ANormalStyle, ADownStyle, AFocusedStyle,
      AHotStyle, ADisabledStyle: TStyledButtonAttributes);
    function ButtonFamilyName: string;
    function GetButtonClasses: TButtonClasses;
    function GetButtonAppearances: TButtonAppearances;
  end;

  TButtonFamily = class(TObject)
  private
    FStyleFamily: TStyledButtonFamily;
    FStyledAttributes: IStyledButtonAttributes;
  end;

//utilities
function DarkenColor(Color:TColor; Percent:integer):TColor;
function LightenColor(Color:TColor; Percent:integer):TColor;
function HtmlToColor(Color: string): TColor;
function ColortoGrayscale(AColor : TColor): TColor;
function ColorIsLight(Color: TColor): Boolean;
function SameStyledButtonStyle(Style1, Style2: TStyledButtonAttributes): Boolean;
procedure CloneButtonStyle(const ASource: TStyledButtonAttributes;
  var ADest: TStyledButtonAttributes);

//drawing procedure
procedure CanvasDrawRoundRect(const ACanvas: TCanvas; ARect: TRect; ARadius: Integer);
procedure CanvasDrawCircle(const ACanvas: TCanvas; ARect: TRect);
procedure CanvasDrawRect(const ACanvas: TCanvas; ARect: TRect);

//ButtonFamily Factory
procedure RegisterButtonFamily(
  const AStyledButtonAttributes: IStyledButtonAttributes);
function GetButtonFamilies: TObjectList;
function GetButtonFamilyName(const Index: Integer): TStyledButtonFamily;
function GetButtonFamilyClasses(const AFamily: TStyledButtonFamily): TButtonClasses;
function GetButtonFamilyAppearances(const AFamily: TStyledButtonFamily): TButtonAppearances;

procedure StyleFamilyCheckAttributes(
  const AFamily: TStyledButtonFamily;
  var AClass: TStyledButtonClass;
  var AAppearance: TStyledButtonAppearance);

procedure StyleFamilyUpdateAttributes(
  const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var ANormalStyle, ADownStyle,
  AFocusedStyle, AHotStyle, ADisabledStyle: TStyledButtonAttributes);

implementation

uses
  Winapi.Windows
{$ifdef GDIPlusSupport}
  , Winapi.GDIPAPI
  , Winapi.GDIPOBJ
{$endif}
  , System.SysUtils
  , System.UITypes
  , System.Math;

function SameStyledButtonStyle(Style1, Style2: TStyledButtonAttributes): Boolean;
begin
  Result :=
    (Style1.BorderType  = Style2.BorderType ) and
    (Style1.BorderWidth = Style2.BorderWidth) and
    (Style1.BorderStyle = Style2.BorderStyle) and
    (Style1.BorderColor = Style2.BorderColor) and
    (Style1.FontColor   = Style2.FontColor  ) and
    (Style1.FontStyle   = Style2.FontStyle  ) and
    (Style1.FontName    = Style2.FontName   ) and
    (Style1.ButtonColor = Style2.ButtonColor);
end;

function ColortoGrayscale(AColor : TColor): TColor;
var
  LGray : byte;
begin
  // Ignore reserved color values : "INHERIT" (TColors.SysDefault) and "none" (TColors.SysNone) .
  if (AColor = TColors.SysDefault) or (AColor = TColors.SysNone) then exit(aColor);

  // get the luminance according to https://www.w3.org/TR/AERT/#color-contrast
  LGray  := round((0.299 * GetRValue(aColor)) + (0.587 * GetGValue(aColor)) + (0.114 * GetBValue(aColor)));

  // set the result to the new grayscale color including the alpha info
  Result := (aColor and $FF000000) or rgb(LGray, LGray, LGray);
end;

function HtmlToColor(Color: string): TColor;
begin
  Result := StringToColor('$' + Copy(Color, 6, 2) + Copy(Color, 4, 2) + Copy(Color, 2, 2));
end;

function DarkenColor(Color:TColor; Percent:Integer):TColor;
var r,g,b:Byte;
begin
  Color:=ColorToRGB(Color);
  r:=GetRValue(Color);
  g:=GetGValue(Color);
  b:=GetBValue(Color);
  r:=r-muldiv(r,Percent,100);  //Percent% closer to black
  g:=g-muldiv(g,Percent,100);
  b:=b-muldiv(b,Percent,100);
  result:=RGB(r,g,b);
end;

function LightenColor(Color:TColor; Percent:Integer):TColor;
var r,g,b:Byte;
begin
  Color:=ColorToRGB(Color);
  r:=GetRValue(Color);
  g:=GetGValue(Color);
  b:=GetBValue(Color);
  r:=r+muldiv(255-r,Percent,100); //Percent% closer to white
  g:=g+muldiv(255-g,Percent,100);
  b:=b+muldiv(255-b,Percent,100);
  result:=RGB(r,g,b);
end;

function ColorIsLight(Color: TColor): Boolean;
begin
  Color := ColorToRGB(Color);
  Result := ((Color and $FF) + (Color shr 8 and $FF) +
  (Color shr 16 and $FF))>= $180;
end;

procedure CloneButtonStyle(
  const ASource: TStyledButtonAttributes;
  var ADest: TStyledButtonAttributes);
begin
  ADest.BorderType := ASource.BorderType;
  ADest.BorderWidth := ASource.BorderWidth;
  ADest.BorderStyle := ASource.BorderStyle;
  ADest.BorderColor := ASource.BorderColor;
  ADest.FontName := ASource.FontName;
  ADest.FontColor := ASource.FontColor;
  ADest.FontStyle := ASource.FontStyle;
  ADest.ButtonColor := ASource.ButtonColor;
end;

var
  FFamilies: TObjectList;

function GetButtonFamily(const AFamily: TStyledButtonFamily;
  out AButtonFamily: TButtonFamily): Boolean;
var
  I: Integer;
  LButtonFamily: TButtonFamily;
begin
  Result := False;
  for I := 0 to FFamilies.Count -1 do
  begin
    LButtonFamily := FFamilies.Items[I] as TButtonFamily;
    if SameText(LButtonFamily.FStyleFamily, AFamily) then
    begin
      AButtonFamily := LButtonFamily;
      Result := True;
      Exit;
    end;
    AButtonFamily := nil;
  end;
end;

procedure StyleFamilyCheckAttributes(
  const AFamily: TStyledButtonFamily;
  var AClass: TStyledButtonClass;
  var AAppearance: TStyledButtonAppearance);
var
  I: Integer;
  LButtonFamily: TButtonFamily;
  LClasses: TButtonClasses;
  LAppearances: TButtonAppearances;
  LDefaultClass: TStyledButtonClass;
  LDefaultAppearance: TStyledButtonAppearance;
  LClassFound, LAppearanceFound: Boolean;
begin
  if GetButtonFamily(AFamily, LButtonFamily) then
  begin
    LClasses := LButtonFamily.FStyledAttributes.GetButtonClasses;
    LDefaultClass := LClasses[0];
    LClassFound := False;
    //Check AClass
    for I := 0 to Length(LClasses)-1 do
    begin
      if SameText(LClasses[I], AClass) then
      begin
        AClass := LClasses[I];
        LClassFound := True;
        break;
      end;
    end;
    if not LClassFound then
      AClass := LDefaultClass;

    LAppearances := LButtonFamily.FStyledAttributes.GetButtonAppearances;
    LDefaultAppearance := LAppearances[0];
    LAppearanceFound := False;
    //Check AAppearance
    for I := 0 to Length(LAppearances)-1 do
    begin
      if SameText(LAppearances[I], AAppearance) then
      begin
        AAppearance := LAppearances[I];
        LAppearanceFound := True;
        break;
      end;
    end;
    if not LAppearanceFound then
      AAppearance := LDefaultAppearance;
  end;
end;

procedure StyleFamilyUpdateAttributes(
  const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var ANormalStyle, ADownStyle,
  AFocusedStyle, AHotStyle, ADisabledStyle: TStyledButtonAttributes);
var
  LButtonFamily: TButtonFamily;
begin
  if GetButtonFamily(AFamily, LButtonFamily) then
  begin
    LButtonFamily.FStyledAttributes.UpdateAttributes(
      AFamily, AClass, AAppearance,
      ANormalStyle, ADownStyle, AFocusedStyle,
      AHotStyle, ADisabledStyle);

    //Attributes defined with Family/Class/Appearance reset any changes
    ANormalStyle.ResetChanged;
    ADownStyle.ResetChanged;
    AFocusedStyle.ResetChanged;
    AHotStyle.ResetChanged;
    ADisabledStyle.ResetChanged;
  end;
end;

procedure RegisterButtonFamily(
  const AStyledButtonAttributes: IStyledButtonAttributes);
var
  LFamily: TButtonFamily;
begin
  LFamily := TButtonFamily.Create;
  LFamily.FStyleFamily := AStyledButtonAttributes.ButtonFamilyName;
  LFamily.FStyledAttributes := AStyledButtonAttributes;
  FFamilies.Add(LFamily);
end;

function GetButtonFamilies: TObjectList;
begin
  Result := FFamilies;
end;

function GetButtonFamilyName(const Index: Integer): TStyledButtonFamily;
begin
  Result := (FFamilies.Items[Index] as TButtonFamily).FStyleFamily;
end;

function GetButtonFamilyClasses(const AFamily: TStyledButtonFamily): TButtonClasses;
var
  LButtonFamily: TButtonFamily;
begin
  if GetButtonFamily(AFamily, LButtonFamily) then
    Result := LButtonFamily.FStyledAttributes.GetButtonClasses
  else
    raise Exception.CreateFmt(ERROR_FAMILY_NOT_FOUND,[AFamily]);
end;

function GetButtonFamilyAppearances(const AFamily: TStyledButtonFamily): TButtonAppearances;
var
  LButtonFamily: TButtonFamily;
begin
  if GetButtonFamily(AFamily, LButtonFamily) then
    Result := LButtonFamily.FStyledAttributes.GetButtonAppearances
  else
    raise Exception.CreateFmt(ERROR_FAMILY_NOT_FOUND,[AFamily]);
end;

{ TStyledButtonAttributes }

function TStyledButtonAttributes.IsChanged: Boolean;
begin
  Result := FIsChanged;
end;

procedure TStyledButtonAttributes.ResetChanged;
begin
  FIsChanged := False;
end;

procedure TStyledButtonAttributes.Assign(ASource: TPersistent);
var
  LSource: TStyledButtonAttributes;
begin
  if ASource is TStyledButtonAttributes then
  begin
    LSource := TStyledButtonAttributes(ASource);
    FBorderType   := LSource.FBorderType;
    FBorderWidth  := LSource.FBorderWidth;
    FBorderStyle  := LSource.FBorderStyle;
    FBrushStyle   := LSource.FBrushStyle;
    FBorderColor  := LSource.FBorderColor;
    FFontColor    := LSource.FFontColor;
    FFontStyle    := LSource.FFontStyle;
    FFontName     := LSource.FFontName;
    FButtonColor  := LSource.FButtonColor;
    FRadius       := LSource.FRadius;
  end
  else
    inherited Assign(ASource);
end;

constructor TStyledButtonAttributes.Create(AOwner: TComponent);
begin
  inherited;
  FRadius := DEFAULT_RADIUS;
  if AOwner is TGraphicControl then
  begin
    FOwnerControl := TGraphicControl(AOwner);
    SetSubComponent(True);
    FIsChanged := False;
  end;
end;

procedure TStyledButtonAttributes.InvalidateControl;
begin
  if Assigned(FOwnerControl) then
  begin
    FIsChanged := True;
    FOwnerControl.Invalidate;
  end;
end;

procedure TStyledButtonAttributes.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    InvalidateControl;
  end;
end;

procedure TStyledButtonAttributes.SetBorderStyle(const Value: TPenStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    InvalidateControl;
  end;
end;

procedure TStyledButtonAttributes.SetBrushStyle(const Value: TBrushStyle);
begin
  if FBrushStyle <> Value then
  begin
    FBrushStyle := Value;
    InvalidateControl;
  end;
end;

procedure TStyledButtonAttributes.SetBorderType(const Value: TBtnBorder);
begin
  if FBorderType <> Value then
  begin
    FBorderType := Value;
    InvalidateControl;
  end;
end;

procedure TStyledButtonAttributes.SetBorderWidth(const Value: Integer);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    InvalidateControl;
  end;
end;

procedure TStyledButtonAttributes.SetButtonColor(const Value: TColor);
begin
  if FButtonColor <> Value then
  begin
    FButtonColor := Value;
    InvalidateControl;
  end;
end;

procedure TStyledButtonAttributes.SetFontColor(const Value: TColor);
begin
  if FFontColor <> Value then
  begin
    FFontColor := Value;
    InvalidateControl;
  end;
end;

procedure TStyledButtonAttributes.SetFontName(const Value: TFontName);
begin
  if FFontName <> Value then
  begin
    FFontName := Value;
    InvalidateControl;
  end;
end;

procedure TStyledButtonAttributes.SetFontStyle(const Value: TFontStyles);
begin
  if FFontStyle <> Value then
  begin
    FFontStyle := Value;
    InvalidateControl;
  end;
end;

procedure TStyledButtonAttributes.SetRadius(const Value: Integer);
begin
  if FRadius <> Value then
  begin
    FRadius := Value;
    InvalidateControl;
  end;
end;

procedure AdjustCanvasRect(const ACanvas: TCanvas;
  var ARect: TRect; ADrawingRect: Boolean);
var
  LWidth: Integer;
begin
  if not ADrawingRect then
  begin
    //Drawing RoundRect with border
    LWidth := (ACanvas.Pen.Width) div 2;
    InflateRect(ARect, -LWidth, -LWidth);
    if not Odd(ACanvas.Pen.Width) then
    begin
      ARect.Width := ARect.Width +1;
      ARect.Height := ARect.Height +1;
    end;
  end
  else
  begin
    if ACanvas.Pen.Style <> psClear then
    begin
      //Reduce Canvas to draw a Square of Pen.Width
      LWidth := ACanvas.Pen.Width div 2;
      InflateRect(ARect, -LWidth, -LWidth);
      if not Odd(ACanvas.Pen.Width) then
      begin
        ARect.Width := ARect.Width +1;
        ARect.Height := ARect.Height +1;
      end;
    end;
  end;
  if ARect.Height < 2 then
    ARect.Height := 2;
  if ARect.Width < 2 then
    ARect.Width := 2;
end;

procedure GPInflateRectF(var ARect: TGPRectF;
  const AValue: Single);
begin
  ARect.X := ARect.X + (AValue / 2);
  ARect.Y := ARect.Y + (AValue / 2);
  ARect.Width := ARect.width - AValue -1;
  ARect.Height := ARect.Height - AValue -1;
end;

{$ifdef GDIPlusSupport}
function GetRoundRectangle(rectangle: TGPRectF;
  radius: Single): TGPGraphicsPath;
var
  path : TGPGraphicsPath;
  l, t, w, h, d : Single;
begin
  path := TGPGraphicsPath.Create;
  l := rectangle.X;
  t := rectangle.y;
  w := rectangle.Width;
  h := rectangle.Height;
  d := radius / 2;

  // the lines beween the arcs are automatically added by the path
  path.AddArc(l, t, d, d, 180, 90); // topleft
  path.AddArc(l + w - d, t, d, d, 270, 90); // topright
  path.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
  path.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
  path.CloseFigure();
  result := path;
end;

function GPColor(Col: TColor): TGPColor;
var
  ColRef: COLORREF;
begin
  ColRef := ColorToRGB(Col);
  Result := MakeColor(GetRValue(ColRef), GetGValue(ColRef),
  GetBValue(ColRef));
end;

procedure CanvasDrawRoundRect(const ACanvas: TCanvas; ARect: TRect;
  ARadius: Integer);
var
  LGraphics: TGPGraphics;
  LSolidPen: TGPPen;
  LBrush: TGPBrush;
  LColor: TGPColor;
  LRect: TGPRectF;
  Path: TGPGraphicsPath;
  LBorderWidth: Single;
  X, Y, W, H: Single;
begin
  LGraphics := nil;
  LSolidPen := nil;
  try
    X := ARect.Left;
    Y := ARect.Top;
    W := ARect.Width;
    H := ARect.Height;
    LRect := Winapi.GDIPAPI.MakeRect(X, Y, W, H);
    LBorderWidth := ACanvas.Pen.Width;
    //Reduce canvas to draw a rectangle of Pen Width
    GPInflateRectF(LRect, LBorderWidth);
    LGraphics := TGPGraphics.Create(ACanvas.Handle);
    LGraphics.SetSmoothingMode(SmoothingModeAntiAlias);
    LColor := GPColor(ACanvas.Pen.Color);
    Path := GetRoundRectangle(LRect, ARadius*2);
    LBrush := TGPSolidBrush.Create(GPColor(ACanvas.Brush.Color));
    LGraphics.FillPath(LBrush, Path);
    LSolidPen := TGPPen.Create(LColor, LBorderWidth);
    LGraphics.DrawPath(LSolidPen, Path);
  finally
    LGraphics.Free;
    LSolidPen.Free;
  end;
end;

procedure CanvasDrawCircle(const ACanvas: TCanvas; ARect: TRect);
var
  LGraphics: TGPGraphics;
  LSolidPen: TGPPen;
  LBrush: TGPBrush;
  LColor: TGPColor;
  LRect: TGPRectF;
  LBorderWidth: Single;
  X, Y, W, H: Single;
begin
  LGraphics := nil;
  LSolidPen := nil;
  try
    X := ARect.Left;
    Y := ARect.Top;
    W := ARect.Width;
    H := ARect.Height;
    LRect := Winapi.GDIPAPI.MakeRect(X, Y, W, H);
    LBorderWidth := ACanvas.Pen.Width;
    //Reduce canvas to draw a rectangle of Pen Width
    GPInflateRectF(LRect, LBorderWidth);
    LGraphics := TGPGraphics.Create(ACanvas.Handle);
    LGraphics.SetSmoothingMode(SmoothingModeAntiAlias);
    LColor := GPColor(ACanvas.Pen.Color);
    LBrush := TGPSolidBrush.Create(GPColor(ACanvas.Brush.Color));
    LGraphics.FillEllipse(LBrush, LRect);
    LSolidPen := TGPPen.Create(LColor, LBorderWidth);
    LGraphics.DrawEllipse(LSolidPen, LRect);
  finally
    LGraphics.Free;
    LSolidPen.Free;
  end;
end;
{$else}
procedure CanvasDrawCircle(const ACanvas: TCanvas; ARect: TRect);
begin
  ACanvas.Ellipse(ARect.Left, ARect.Top,
    ARect.Left + ARect.Width, ARect.Top + ARect.Height);
end;

procedure CanvasDrawRoundRect(const ACanvas: TCanvas; ARect: TRect;
  ARadius: Integer);
begin
  AdjustCanvasRect(ACanvas, ARect, False);
  ACanvas.Pen.Style := psSolid;
  ACanvas.RoundRect(ARect, ARadius, ARadius);
end;
{$endif}

procedure CanvasDrawRect(const ACanvas: TCanvas; ARect: TRect);
begin
  AdjustCanvasRect(ACanvas, ARect, True);
  ACanvas.Rectangle(ARect);
end;

initialization
  FFamilies := TObjectList.Create(True);

finalization
  FFamilies.Free;

end.
