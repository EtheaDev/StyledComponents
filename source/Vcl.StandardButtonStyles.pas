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
unit Vcl.StandardButtonStyles;

interface

uses
  Vcl.Graphics
  , System.Classes
  , Vcl.Controls;

Type
  TStyledButtonStyle = string;
  TBtnBorder = (btRounded, btedgy);

  TStyledButtonAttributes = class(TComponent)
  private
    FBorderType: TBtnBorder;
    FBorderWidth: Integer;
    FBorderStyle: TPenStyle;
    FBorderColor: TColor;
    FFontColor: TColor;
    FFontStyle: TFontStyles;
    FFontName: TFontName;
    FButtonColor: TColor;
    FOutLine: Boolean;
    FOwnerControl: TGraphicControl;
    FIsChanged: Boolean;
    procedure InvalidateControl;
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderStyle(const Value: TPenStyle);
    procedure SetBorderType(const Value: TBtnBorder);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetButtonColor(const Value: TColor);
    procedure SetFontColor(const Value: TColor);
    procedure SetFontName(const Value: TFontName);
    procedure SetFontStyle(const Value: TFontStyles);
    procedure SetOutLine(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    function IsChanged: Boolean;
  published
    property BorderType : TBtnBorder  read FBorderType   write SetBorderType;
    property BorderWidth: Integer     read FBorderWidth  write SetBorderWidth;
    property BorderStyle: TPenStyle   read FBorderStyle  write SetBorderStyle;
    property BorderColor: TColor      read FBorderColor  write SetBorderColor;
    property FontColor  : TColor      read FFontColor    write SetFontColor;
    property FontStyle  : TFontStyles read FFontStyle    write SetFontStyle;
    property FontName   : TFontName   read FFontName     write SetFontName;
    property ButtonColor: TColor      read FButtonColor  write SetButtonColor;
    property OutLine    : Boolean     read FOutLine      write SetOutLine;
  end;

  //  Abstraction of Graphic Button Attributes
  IStyledButtonAttributes = interface
    ['{C7BC98EE-D513-46B9-881A-2FDE8DE07786}']
    procedure UpdateAttributes(const AStyle: TStyledButtonStyle;
      var ANormalStyle, ADownStyle, AFocusedStyle,
      AHotStyle, ADisabledStyle: TStyledButtonAttributes);
  end;

  TStyledButtonStdStyle = class(TInterfacedObject, IStyledButtonAttributes)
  private
  protected
    procedure AssignStyle(const ASource: TStyledButtonAttributes;
      var ADest: TStyledButtonAttributes); virtual;
    procedure InternalUpdateAttributes(
      const AStyle: TStyledButtonStyle;
      var ANormalStyle, ADownStyle, AFocusedStyle,
      AHotStyle, ADisabledStyle: TStyledButtonAttributes); virtual;
  public
    //IStyledButtonAttributes interface
    procedure UpdateAttributes(
      const AStyle: TStyledButtonStyle;
      var ANormalStyle, ADownStyle, AFocusedStyle,
      AHotStyle, ADisabledStyle: TStyledButtonAttributes);
  end;

function DarkenColor(Color:TColor; Percent:integer):TColor;
function LightenColor(Color:TColor; Percent:integer):TColor;
function HtmlToColor(Color: string): TColor;
function ColortoGrayscale(AColor : TColor): TColor;
function ColorIsLight(Color: TColor): Boolean;
function SameStyledButtonStyle(Style1, Style2: TStyledButtonAttributes): Boolean;

implementation

uses
  Winapi.Windows
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
    (Style1.ButtonColor = Style2.ButtonColor) and
    (Style1.OutLine     = Style2.OutLine    );
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

{ TStyledButtonStdStyle }

procedure TStyledButtonStdStyle.AssignStyle(
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
  ADest.OutLine := ASource.OutLine;
end;

procedure TStyledButtonStdStyle.InternalUpdateAttributes(
  const AStyle: TStyledButtonStyle;
  var ANormalStyle, ADownStyle, AFocusedStyle, AHotStyle,
  ADisabledStyle: TStyledButtonAttributes);
var
  LFontColor, LButtonColor: TColor;
begin
  LFontColor := clBtnText;
  LButtonColor := clBtnFace;
  //Default Style Attributes
  ANormalStyle.BorderType := btRounded;
  ANormalStyle.BorderWidth := 3;
  ANormalStyle.BorderColor := clBtnShadow;
  ANormalStyle.BorderStyle := psSolid;
  ANormalStyle.FontStyle := [];
  ANormalStyle.FontColor := LFontColor;
  ANormalStyle.ButtonColor := LButtonColor;
  //ANormalStyle.BorderColor := ANormalStyle.ButtonColor;
  ANormalStyle.OutLine := False;

  //Clone Normal Style to Other Styles
  AssignStyle(ANormalStyle, ADownStyle);
  AssignStyle(ANormalStyle, AFocusedStyle);
  AssignStyle(ANormalStyle, AHotStyle);
  AssignStyle(ANormalStyle, ADisabledStyle);

  //Button Down
  ADownStyle.ButtonColor := DarkenColor(LButtonColor, 20);
  ADownStyle.BorderColor := LightenColor(LButtonColor, 50);
  ADownStyle.BorderStyle := psSolid;
  ADownStyle.BorderWidth := 1;

  //Button Hot: color as Down but no Border
  AHotStyle.ButtonColor := ADownStyle.ButtonColor;

  //Button Focused
  AFocusedStyle.ButtonColor := DarkenColor(LButtonColor, 20);
  AFocusedStyle.BorderStyle := psSolid;
  AFocusedStyle.BorderWidth := 1;

  //Button Disabled
  ADisabledStyle.ButtonColor := LightenColor(ANormalStyle.ButtonColor, 70);//ColortoGrayscale(LButtonColor);//
  ADisabledStyle.FontColor := LightenColor(LFontColor, 70);
end;

procedure TStyledButtonStdStyle.UpdateAttributes(
  const AStyle: TStyledButtonStyle;
  var ANormalStyle, ADownStyle, AFocusedStyle,
  AHotStyle, ADisabledStyle: TStyledButtonAttributes);
begin
  InternalUpdateAttributes(AStyle,
    ANormalStyle, ADownStyle, AFocusedStyle,
    AHotStyle, ADisabledStyle);
  ANormalStyle.FIsChanged := False;
  ADownStyle.FIsChanged := False;
  AFocusedStyle.FIsChanged := False;
  AHotStyle.FIsChanged := False;
  ADisabledStyle.FIsChanged := False;
end;

{ TStyledButtonAttributes }

function TStyledButtonAttributes.IsChanged: Boolean;
begin
  Result := FIsChanged;
end;

constructor TStyledButtonAttributes.Create(AOwner: TComponent);
begin
  inherited;
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

procedure TStyledButtonAttributes.SetOutLine(const Value: Boolean);
begin
  if FOutLine <> Value then
  begin
    FOutLine := Value;
    InvalidateControl;
  end;
end;

end.
