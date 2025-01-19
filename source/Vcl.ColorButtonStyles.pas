{******************************************************************************}
{                                                                              }
{  ColorButtonStyles: Button Styles based on VCL color names                   }
{  Unit System.UIConsts                                                        }
{                                                                              }
{  Copyright (c) 2022-2025 (Ethea S.r.l.)                                      }
{  Author: Carlo Barazzetta                                                    }
{  Contributors:                                                               }
{                                                                              }
{  https://github.com/EtheaDev/StyledComponents                                }
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
unit Vcl.ColorButtonStyles;

interface

uses
  Vcl.Graphics
  , System.UIConsts
  , System.UITypes
  , Vcl.ButtonStylesAttributes;

const
  //Button Family
  BASIC_COLOR_FAMILY = 'Basic-Colors';
  SVG_COLOR_FAMILY = 'SVG-Colors';

  //Button Class contains the name of the color, for example:
  //for 'BasicColors': clBlack, clRed, clYellow (Colors: array[0..51] in UIConsts)
  //for 'SvgColors': claAliceblue, claAntiquewhite, claAqua (AlphaColors: array [0..147] in UIConsts)

  //Button Appearance
  COLOR_BTN_NORMAL = 'Normal';
  COLOR_BTN_OUTLINE = 'Outline';
  COLOR_BTN_WIDTH = 2;

Type
  TBasicColorButtonStyles = class(TInterfacedObject, IStyledButtonAttributes)
  private
    procedure UpdateAttributes(
      const AFamily:  TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance;
      var ANormalStyle, APressedStyle, ASelectedStyle,
      AHotStyle, ADisabledStyle: TStyledButtonAttributes);
    function ButtonFamilyName: string;
    function GetButtonClasses: TButtonClasses;
    function GetButtonAppearances: TButtonAppearances;
    procedure GetStyleByModalResult(
      const AModalResult: System.UITypes.TModalResult;
      var AStyleClass: TStyledButtonClass;
      var AStyleAppearance: TStyledButtonAppearance);
    procedure GetColorName(const AColorName: string);

    procedure BasicClassToColors(const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance; var AFontColor,
      AButtonColor, ABorderColor: TColor; out AOutLine: Boolean);
  end;

  TSVGColorButtonStyles = class(TInterfacedObject, IStyledButtonAttributes)
  private
    procedure UpdateAttributes(
      const AFamily:  TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance;
      var ANormalStyle, APressedStyle, ASelectedStyle,
      AHotStyle, ADisabledStyle: TStyledButtonAttributes);
    function ButtonFamilyName: string;
    function GetButtonClasses: TButtonClasses;
    function GetButtonAppearances: TButtonAppearances;
    procedure GetStyleByModalResult(
      const AModalResult: System.UITypes.TModalResult;
      var AStyleClass: TStyledButtonClass;
      var AStyleAppearance: TStyledButtonAppearance);
    procedure GetColorName(const AColorName: string);

    procedure SVGClassToColors(const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance; var AFontColor,
      AButtonColor, ABorderColor: TColor; out AOutLine: Boolean);
  end;

implementation

uses
  System.SysUtils
  , WinApi.Windows;

var
  BasicButtonClasses: TButtonClasses;
  SvgButtonClasses: TButtonClasses;
  ButtonAppearances: TButtonAppearances;

{ TBasicColorButtonStyles }

procedure TBasicColorButtonStyles.BasicClassToColors(const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var AFontColor, AButtonColor, ABorderColor: TColor; out AOutLine: Boolean);
var
  LColor: integer;
begin
  AOutLine := SameText(AAppearance, COLOR_BTN_OUTLINE);
  IdentToColor(AClass, LColor);
  AButtonColor := TColor(LColor);
  if ColorIsLight(AButtonColor) then
  begin
    ABorderColor := DarkenColor(AButtonColor, 20);
    AFontColor := clBlack;
  end
  else
  begin
    ABorderColor := LightenColor(AButtonColor, 20);
    AFontColor := clWhite;
  end;
end;

function TBasicColorButtonStyles.ButtonFamilyName: string;
begin
  Result := BASIC_COLOR_FAMILY;
end;

function TBasicColorButtonStyles.GetButtonAppearances: TButtonAppearances;
begin
  Result := ButtonAppearances;
end;

procedure TBasicColorButtonStyles.GetColorName(const AColorName: string);
begin
  SetLength(BasicButtonClasses, Length(BasicButtonClasses)+1);
  BasicButtonClasses[Length(BasicButtonClasses)-1] := AColorName;
end;

function TBasicColorButtonStyles.GetButtonClasses: TButtonClasses;
begin
  if Length(BasicButtonClasses) = 0 then
    GetColorValues(GetColorName);
  Result := BasicButtonClasses;
end;

procedure TBasicColorButtonStyles.GetStyleByModalResult(
  const AModalResult: System.UITypes.TModalResult;
  var AStyleClass: TStyledButtonClass;
  var AStyleAppearance: TStyledButtonAppearance);
begin
  //if AModalResult is mrNone, define the defaults of Family
  case AModalResult of
    mrNone     : begin AStyleClass := 'clBlue'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrYes      : begin AStyleClass := 'clBlue'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrNo       : begin AStyleClass := 'clGray'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrOk       : begin AStyleClass := 'clGreen'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrCancel   : begin AStyleClass := 'clRed'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrAbort    : begin AStyleClass := 'clRed'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrRetry    : begin AStyleClass := 'clYellow'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrIgnore   : begin AStyleClass := 'clGray'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrAll      : begin AStyleClass := 'clBlue'; AStyleAppearance := COLOR_BTN_OUTLINE; end;
    mrNoToAll  : begin AStyleClass := 'clRed'; AStyleAppearance := COLOR_BTN_OUTLINE; end;
    mrYesToAll : begin AStyleClass := 'clGreen'; AStyleAppearance := COLOR_BTN_OUTLINE; end;
    mrClose    : begin AStyleClass := 'clGray'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrTryAgain : begin AStyleClass := 'clYellow'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrContinue : begin AStyleClass := 'clGray'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrHelp     : begin AStyleClass := 'clYellow'; AStyleAppearance := COLOR_BTN_NORMAL; end;
  else
    GetStyleByModalResult(mrNone, AStyleClass, AStyleAppearance);
  end;
end;

procedure TBasicColorButtonStyles.UpdateAttributes(
  const AFamily:  TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var ANormalStyle, APressedStyle, ASelectedStyle, AHotStyle,
  ADisabledStyle: TStyledButtonAttributes);
var
  LFontColor, LButtonColor, LBorderColor: TColor;
  LOutLine: Boolean;
begin
  BasicClassToColors(AClass, AAppearance, LFontColor, LButtonColor, LBorderColor, LOutLine);

  //Default Style Attributes for Basic Color Buttons
  ANormalStyle.DrawType := btRoundRect;
  ANormalStyle.FontStyle := [fsBold];
  ANormalStyle.BorderWidth := COLOR_BTN_WIDTH;

  //Style for Normal Style of Basic Color Button
  if LOutLine then
  begin
    //Outline: Border and FontColor same as Button Color
    ANormalStyle.ButtonDrawStyle := btnClear;
    ANormalStyle.BorderDrawStyle := brdSolid;
    ANormalStyle.BorderWidth := COLOR_BTN_WIDTH;
    ANormalStyle.BorderColor := LButtonColor;
    ANormalStyle.FontColor := LButtonColor;
  end
  else
  begin
    ANormalStyle.ButtonDrawStyle := btnSolid;
    ANormalStyle.BorderDrawStyle := brdSolid;
    ANormalStyle.FontColor := LFontColor;
    ANormalStyle.ButtonColor := LButtonColor;
    ANormalStyle.BorderColor := LBorderColor;
  end;

  //Clone Normal Style to Other Styles
  CloneButtonStyle(ANormalStyle, APressedStyle);
  CloneButtonStyle(ANormalStyle, ASelectedStyle);
  CloneButtonStyle(ANormalStyle, AHotStyle);
  CloneButtonStyle(ANormalStyle, ADisabledStyle);

  if LOutline then
  begin
    //Button Down: color as Button Color
    with APressedStyle do
    begin
      ButtonColor := DarkenColor(LButtonColor, 20);
      BorderColor := LBorderColor;
      BorderDrawStyle := brdSolid;
      BorderWidth := COLOR_BTN_WIDTH+2;
      FontColor   := LFontColor;
      ButtonDrawStyle  := btnSolid;
    end;

    //Button Hot: color as Button Color
    with AHotStyle do
    begin
      ButtonColor := LButtonColor;
      BorderDrawStyle := brdClear;
      BorderWidth := COLOR_BTN_WIDTH;
      FontColor := LFontColor;
      ButtonDrawStyle  := btnSolid;
    end;

    //Button Focused
    with ASelectedStyle do
    begin
      ButtonColor := LButtonColor;
      FontColor := LFontColor;
      BorderDrawStyle := brdSolid;
      BorderWidth := COLOR_BTN_WIDTH+1;
      BorderColor := LBorderColor;
      ButtonDrawStyle  := btnSolid;
    end;

    //Button Disabled
    ADisabledStyle.BorderColor := LightenColor(ADisabledStyle.BorderColor, 50);
    ADisabledStyle.FontColor := LightenColor(ADisabledStyle.FontColor, 50);
  end
  else
  begin
    //Button Down
    APressedStyle.ButtonColor := LBorderColor;
    APressedStyle.BorderColor := LButtonColor;
    APressedStyle.BorderDrawStyle := brdSolid;
    APressedStyle.BorderWidth := COLOR_BTN_WIDTH;

    //Button Hot: color as Down but no Border
    AHotStyle.ButtonColor := APressedStyle.ButtonColor;

    //Button Focused
    ASelectedStyle.ButtonColor := APressedStyle.ButtonColor;
    ASelectedStyle.BorderDrawStyle := brdSolid;
    ASelectedStyle.BorderWidth := COLOR_BTN_WIDTH;
  end;

  //Disabled Button (lighten)
  ADisabledStyle.BorderColor := LightenColor(ADisabledStyle.BorderColor, 50);
  ADisabledStyle.ButtonColor := LightenColor(ADisabledStyle.ButtonColor, 50);
  ADisabledStyle.FontColor := LightenColor(ADisabledStyle.FontColor, 50);
end;

{ TSVGColorButtonStyles }

procedure TSVGColorButtonStyles.SVGClassToColors(const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var AFontColor, AButtonColor, ABorderColor: TColor; out AOutLine: Boolean);
var
  LColor: TAlphaColor;

  function AlphaColorToColor(const Color: TAlphaColor): TColor;
  begin
    TColorRec(Result).R := TAlphaColorRec(Color).R;
    TColorRec(Result).G := TAlphaColorRec(Color).G;
    TColorRec(Result).B := TAlphaColorRec(Color).B;
    TColorRec(Result).A := 0;
  end;

begin
  AOutLine := SameText(AAppearance, COLOR_BTN_OUTLINE);
  LColor := StringToAlphaColor(AClass);
  AButtonColor := AlphaColorToColor(LColor);
  if ColorIsLight(AButtonColor) then
  begin
    ABorderColor := DarkenColor(AButtonColor, 20);
    AFontColor := clBlack;
  end
  else
  begin
    ABorderColor := LightenColor(AButtonColor, 20);
    AFontColor := clWhite;
  end;
end;

function TSVGColorButtonStyles.ButtonFamilyName: string;
begin
  Result := SVG_COLOR_FAMILY;
end;

function TSVGColorButtonStyles.GetButtonAppearances: TButtonAppearances;
begin
  Result := ButtonAppearances;
end;

function TSVGColorButtonStyles.GetButtonClasses: TButtonClasses;
begin
  if Length(SvgButtonClasses) = 0 then
    GetAlphaColorValues(GetColorName);
  Result := SvgButtonClasses;
end;

procedure TSVGColorButtonStyles.GetColorName(const AColorName: string);
begin
  SetLength(SvgButtonClasses, Length(SvgButtonClasses)+1);
  SvgButtonClasses[Length(SvgButtonClasses)-1] := AColorName;
end;

procedure TSVGColorButtonStyles.GetStyleByModalResult(
  const AModalResult: System.UITypes.TModalResult;
  var AStyleClass: TStyledButtonClass;
  var AStyleAppearance: TStyledButtonAppearance);
begin
  //if AModalResult is mrNone, define the defaults of Family
  case AModalResult of
    mrNone     : begin AStyleClass := 'Aliceblue'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrYes      : begin AStyleClass := 'Darkblue'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrNo       : begin AStyleClass := 'Slateblue'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrOk       : begin AStyleClass := 'Forestgreen'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrCancel   : begin AStyleClass := 'Darkred'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrAbort    : begin AStyleClass := 'Firebrick'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrRetry    : begin AStyleClass := 'Gold'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrIgnore   : begin AStyleClass := 'Silver'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrAll      : begin AStyleClass := 'Midnightblue'; AStyleAppearance := COLOR_BTN_OUTLINE; end;
    mrNoToAll  : begin AStyleClass := 'Slateblue'; AStyleAppearance := COLOR_BTN_OUTLINE; end;
    mrYesToAll : begin AStyleClass := 'Darkblue'; AStyleAppearance := COLOR_BTN_OUTLINE; end;
    mrClose    : begin AStyleClass := 'Thistle'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrTryAgain : begin AStyleClass := 'Slategray'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrContinue : begin AStyleClass := 'Lightseagreen'; AStyleAppearance := COLOR_BTN_NORMAL; end;
    mrHelp     : begin AStyleClass := 'Plum'; AStyleAppearance := COLOR_BTN_NORMAL; end;
  else
    GetStyleByModalResult(mrNone, AStyleClass, AStyleAppearance);
  end;
end;

procedure TSVGColorButtonStyles.UpdateAttributes(
  const AFamily: TStyledButtonFamily; const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance; var ANormalStyle, APressedStyle,
  ASelectedStyle, AHotStyle, ADisabledStyle: TStyledButtonAttributes);
var
  LFontColor, LButtonColor, LBorderColor: TColor;
  LOutLine: Boolean;
begin
  SVGClassToColors(AClass, AAppearance, LFontColor, LButtonColor, LBorderColor, LOutLine);

  //Default Style Attributes for Basic Color Buttons
  ANormalStyle.DrawType := btRoundRect;
  ANormalStyle.FontStyle := [fsBold];
  ANormalStyle.BorderWidth := COLOR_BTN_WIDTH;

  //Style for Normal Style of Basic Color Button
  if LOutLine then
  begin
    //Outline: Border and FontColor same as Button Color
    ANormalStyle.ButtonDrawStyle := btnClear;
    ANormalStyle.BorderDrawStyle := brdSolid;
    ANormalStyle.BorderWidth := COLOR_BTN_WIDTH;
    ANormalStyle.BorderColor := LButtonColor;
    ANormalStyle.FontColor := LButtonColor;
  end
  else
  begin
    ANormalStyle.ButtonDrawStyle := btnSolid;
    ANormalStyle.BorderDrawStyle := brdSolid;
    ANormalStyle.FontColor := LFontColor;
    ANormalStyle.ButtonColor := LButtonColor;
    ANormalStyle.BorderColor := LBorderColor;
  end;

  //Clone Normal Style to Other Styles
  CloneButtonStyle(ANormalStyle, APressedStyle);
  CloneButtonStyle(ANormalStyle, ASelectedStyle);
  CloneButtonStyle(ANormalStyle, AHotStyle);
  CloneButtonStyle(ANormalStyle, ADisabledStyle);

  if LOutline then
  begin
    //Button Down: color as Button Color
    with APressedStyle do
    begin
      ButtonColor := DarkenColor(LButtonColor, 20);
      BorderColor := LBorderColor;
      BorderDrawStyle := brdSolid;
      BorderWidth := COLOR_BTN_WIDTH+2;
      FontColor   := LFontColor;
      ButtonDrawStyle  := btnSolid;
    end;

    //Button Hot: color as Button Color
    with AHotStyle do
    begin
      ButtonColor := LButtonColor;
      BorderDrawStyle := brdClear;
      BorderWidth := COLOR_BTN_WIDTH;
      FontColor := LFontColor;
      ButtonDrawStyle  := btnSolid;
    end;

    //Button Focused
    with ASelectedStyle do
    begin
      ButtonColor := LButtonColor;
      FontColor := LFontColor;
      BorderDrawStyle := brdSolid;
      BorderWidth := COLOR_BTN_WIDTH+1;
      BorderColor := LBorderColor;
      ButtonDrawStyle  := btnSolid;
    end;

    //Button Disabled
    ADisabledStyle.BorderColor := LightenColor(ADisabledStyle.BorderColor, 50);
    ADisabledStyle.FontColor := LightenColor(ADisabledStyle.FontColor, 50);
  end
  else
  begin
    //Button Down
    APressedStyle.ButtonColor := LBorderColor;
    APressedStyle.BorderColor := LButtonColor;
    APressedStyle.BorderDrawStyle := brdSolid;
    APressedStyle.BorderWidth := COLOR_BTN_WIDTH;

    //Button Hot: color as Down but no Border
    AHotStyle.ButtonColor := APressedStyle.ButtonColor;

    //Button Focused
    ASelectedStyle.ButtonColor := APressedStyle.ButtonColor;
    ASelectedStyle.BorderDrawStyle := brdSolid;
    ASelectedStyle.BorderWidth := COLOR_BTN_WIDTH;
  end;

  //Disabled Button (lighten)
  ADisabledStyle.BorderColor := LightenColor(ADisabledStyle.BorderColor, 50);
  ADisabledStyle.ButtonColor := LightenColor(ADisabledStyle.ButtonColor, 50);
  ADisabledStyle.FontColor := LightenColor(ADisabledStyle.FontColor, 50);
end;

initialization
  SetLength(ButtonAppearances, 2);
  ButtonAppearances[0] := COLOR_BTN_NORMAL;
  ButtonAppearances[1] := COLOR_BTN_OUTLINE;

  RegisterButtonFamily(TBasicColorButtonStyles.Create);
  RegisterButtonFamily(TSvgColorButtonStyles.Create);

end.
