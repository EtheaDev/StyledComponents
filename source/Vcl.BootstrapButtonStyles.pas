{******************************************************************************}
{                                                                              }
{       BootstrapButtonStyles: Button Styles inspired to Bootstrap             }
{       https://getbootstrap.com/docs/4.0/components/buttons/                  }
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
unit Vcl.BootstrapButtonStyles;

interface

uses
  Vcl.Graphics,
  Vcl.ButtonStylesAttributes;

const
  BOOTSTRAP_BORDER_WIDTH = 2;

  btn_primary = 'Primary';
  btn_secondary = 'Secondary';
  btn_success = 'Success';
  btn_danger = 'Danger';
  btn_warning = 'Warning';
  btn_info =  'Info';
  btn_light = 'Light';
  btn_dark = 'Dark';

Type
  TBoostrapButtonStyles = class(TInterfacedObject, IStyledButtonAttributes)
  private
    procedure UpdateAttributes(
      const AFamily:  TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance;
      var ANormalStyle, ADownStyle, ASelectedStyle,
      AHotStyle, ADisabledStyle: TStyledButtonAttributes);
    function ButtonFamilyName: string;
    function GetButtonClasses: TButtonClasses;
    function GetButtonAppearances: TButtonAppearances;
  end;

implementation

uses
  System.SysUtils
  , WinApi.Windows;

var
  ButtonClasses: TButtonClasses;
  ButtonAppearances: TButtonAppearances;

{ TBoostrapButtonStyles }

procedure BootstrapClassToColors(const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var AFontColor, AButtonColor: TColor; out OutLine: Boolean);

const
  //from bootstrap css
  bs_blue = '#0d6efd';
  bs_indigo = '#6610f2';
  bs_purple = '#6f42c1';
  bs_pink = '#d63384';
  bs_red = '#dc3545';
  bs_orange = '#fd7e14';
  bs_yellow = '#ffc107';
  bs_green = '#198754';
  bs_teal = '#20c997';
  bs_cyan = '#0dcaf0';
  bs_white = '#fff';
  bs_gray = '#6c757d';
  bs_gray_dark = '#343a40';
  bs_primary = '#0d6efd';
  bs_secondary = '#6c757d';
  bs_success = '#198754';
  bs_info = '#0dcaf0';
  bs_warning = '#ffc107';
  bs_danger = '#dc3545';
  bs_light = '#f8f9fa';
  bs_dark = '#212529';

begin
  OutLine := SameText(AAppearance, 'Outline');


  if SameText(AClass, btn_primary) then
  begin
    AButtonColor := htmlToColor(bs_primary);
    AFontColor := clWhite;
  end
  else if SameText(AClass, btn_secondary) then
  begin
    AButtonColor := htmlToColor(bs_secondary);
    AFontColor := clWhite;
  end
  else if SameText(AClass, btn_success) then
  begin
    AButtonColor := htmlToColor(bs_success);
    AFontColor := clWhite;
  end
  else if SameText(AClass, btn_danger) then
  begin
    AButtonColor := htmlToColor(bs_danger);
    AFontColor := clWhite;
  end
  else if SameText(AClass, btn_warning) then
  begin
    AButtonColor := htmlToColor(bs_warning);
    AFontColor := htmlToColor('#212529');
  end
  else if SameText(AClass, btn_info) then
  begin
    AButtonColor := htmlToColor(bs_info);
    AFontColor := htmlToColor('#212529');
  end
  else if SameText(AClass, btn_light) then
  begin
    AButtonColor := htmlToColor(bs_light);
    AFontColor := htmlToColor('#212529');
  end
  else if SameText(AClass, btn_dark) then
  begin
    AButtonColor := htmlToColor(bs_dark);
    AFontColor := clWhite;
  end
  else
  begin
    OutLine := False;
    AButtonColor := clBtnFace;
    AFontColor := clBtnText;
  end;
end;

function TBoostrapButtonStyles.ButtonFamilyName: string;
begin
  Result := 'Bootstrap';
end;

function TBoostrapButtonStyles.GetButtonAppearances: TButtonAppearances;
begin
  Result := ButtonAppearances;
end;

function TBoostrapButtonStyles.GetButtonClasses: TButtonClasses;
begin
  Result := ButtonClasses;
end;

procedure TBoostrapButtonStyles.UpdateAttributes(
  const AFamily:  TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var ANormalStyle, ADownStyle, ASelectedStyle, AHotStyle,
  ADisabledStyle: TStyledButtonAttributes);
var
  LFontColor, LButtonColor: TColor;
  LOutLine: Boolean;
begin
  BootstrapClassToColors(AClass, AAppearance, LFontColor, LButtonColor, LOutLine);

  //Default Style Attributes for Bootstrap Buttons
  ANormalStyle.DrawType := btRounded;
  ANormalStyle.FontStyle := [fsBold];
  ANormalStyle.FontName := 'Tahoma';

  //Style for Normal Style of Bootstrap Button
  if LOutLine then
  begin
    //Outline: Border and FontColor same as Button Color
    ANormalStyle.ButtonDrawStyle := btnClear;
    ANormalStyle.BorderDrawStyle := brdSolid;
    ANormalStyle.BorderWidth := BOOTSTRAP_BORDER_WIDTH;
    ANormalStyle.FontColor := LButtonColor;
    ANormalStyle.BorderColor := LButtonColor;
  end
  else
  begin
    ANormalStyle.BorderDrawStyle := brdClear;
    ANormalStyle.FontColor := LFontColor;
    ANormalStyle.ButtonColor := LButtonColor;
    ANormalStyle.BorderColor := ANormalStyle.ButtonColor;
  end;

  //Clone Normal Style to Other Styles
  CloneButtonStyle(ANormalStyle, ADownStyle);
  CloneButtonStyle(ANormalStyle, ASelectedStyle);
  CloneButtonStyle(ANormalStyle, AHotStyle);
  CloneButtonStyle(ANormalStyle, ADisabledStyle);

  if LOutline then
  begin
    //Button Down: color as Button Color
    with ADownStyle do
    begin
      ButtonColor := LButtonColor;
      BorderColor := LightenColor(LButtonColor, 50);
      BorderDrawStyle := brdSolid;
      BorderWidth := BOOTSTRAP_BORDER_WIDTH;
      FontColor   := LFontColor;
      ButtonDrawStyle  := btnSolid;
    end;

    //Button Hot: color as Button Color
    with AHotStyle do
    begin
      ButtonColor := LButtonColor;
      BorderDrawStyle := brdClear;
      BorderWidth := BOOTSTRAP_BORDER_WIDTH;
      FontColor := LFontColor;
      ButtonDrawStyle  := btnSolid;
    end;

    //Button Focused
    with ASelectedStyle do
    begin
      if SameText(AClass, btn_dark) then
        ButtonColor := LightenColor(LButtonColor, 20)
      else
        ButtonColor := DarkenColor(LButtonColor, 20);
      BorderDrawStyle := brdSolid;
      BorderWidth := BOOTSTRAP_BORDER_WIDTH;
      FontColor := LFontColor;
      ButtonDrawStyle  := btnSolid;
    end;

    //Button Disabled
    with ADisabledStyle do
    begin
      ButtonColor := LightenColor(ANormalStyle.ButtonColor, 70); //ColortoGrayscale(LButtonColor);
      FontColor := LightenColor(LFontColor, 70);
    end;
  end
  else
  begin
    //Button Down
    ADownStyle.ButtonColor := DarkenColor(LButtonColor, 20);
    ADownStyle.BorderColor := LightenColor(LButtonColor, 50);
    ADownStyle.BorderDrawStyle := brdSolid;
    ADownStyle.BorderWidth := BOOTSTRAP_BORDER_WIDTH;

    //Button Hot: color as Down but no Border
    AHotStyle.ButtonColor := ADownStyle.ButtonColor;

    //Button Focused
    ASelectedStyle.ButtonColor := DarkenColor(LButtonColor, 20);
    ASelectedStyle.BorderDrawStyle := brdSolid;
    ASelectedStyle.BorderWidth := BOOTSTRAP_BORDER_WIDTH;

    //Button Disabled
    ADisabledStyle.ButtonColor := LightenColor(ANormalStyle.ButtonColor, 70);//ColortoGrayscale(LButtonColor);
    ADisabledStyle.FontColor := LightenColor(LFontColor, 70);
  end;
end;

initialization
  SetLength(ButtonClasses, 8);
  ButtonClasses[0] := btn_primary;
  ButtonClasses[1] := btn_secondary;
  ButtonClasses[2] := btn_success;
  ButtonClasses[3] := btn_danger;
  ButtonClasses[4] := btn_warning;
  ButtonClasses[5] := btn_info;
  ButtonClasses[6] := btn_light;
  ButtonClasses[7] := btn_dark;

  SetLength(ButtonAppearances, 2);
  ButtonAppearances[0] := 'Normal';
  ButtonAppearances[1] := 'Outline';

  RegisterButtonFamily(TBoostrapButtonStyles.Create);

end.
