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
unit Vcl.BootstrapButtonStyles;

interface

uses
  Vcl.Graphics,
  Vcl.StandardButtonStyles;

const
  DEFAULT_DOWN_BORDER_WIDTH = 3;
  DEFAULT_FOCUSED_BORDER_WIDTH = 3;

  btn_primary = 'Primary';
  btn_secondary = 'Secondary';
  btn_success = 'Success';
  btn_danger = 'Danger';
  btn_warning = 'Warning';
  btn_info =  'Info';
  btn_light = 'Light';
  btn_dark = 'Dark';

Type
  TBoostrapButtonStyleEngine = class(TStyledButtonStdStyle)
  protected
    procedure InternalUpdateAttributes(
      const AStyle: TStyledButtonStyle;
      var ANormalStyle, ADownStyle, AFocusedStyle,
      AHotStyle, ADisabledStyle: TStyledButtonAttributes); override;
  end;

implementation

uses
  System.SysUtils
  , WinApi.Windows;

Type
  TBootstrapAttrib = Record
    ButtonColor: TColor;
    FontColor: TColor;
  End;

{ TBoostrapButtonStyleEngine }

procedure BootstrapClassToColors(const AStyle: TStyledButtonStyle;
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

var
  LStyle: string;
begin
  OutLine := SameText(Copy(AStyle,1,8), 'outline-');
  if OutLine then
    LStyle := Copy(AStyle,9,MaxInt)
  else
    LStyle := AStyle;

  if LStyle = btn_primary then
  begin
    AButtonColor := htmlToColor(bs_primary);
    AFontColor := clWhite;
  end
  else if LStyle = btn_secondary then
  begin
    AButtonColor := htmlToColor(bs_secondary);
    AFontColor := clWhite;
  end
  else if LStyle = btn_success then
  begin
    AButtonColor := htmlToColor(bs_success);
    AFontColor := clWhite;
  end
  else if LStyle = btn_danger then
  begin
    AButtonColor := htmlToColor(bs_danger);
    AFontColor := clWhite;
  end
  else if LStyle = btn_warning then
  begin
    AButtonColor := htmlToColor(bs_warning);
    AFontColor := clBlack;
  end
  else if LStyle = btn_info then
  begin
    AButtonColor := htmlToColor(bs_info);
    AFontColor := clBlack;
  end
  else if LStyle = btn_light then
  begin
    AButtonColor := htmlToColor(bs_light);
    AFontColor := clBlack;
  end
  else if LStyle = btn_dark then
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

procedure TBoostrapButtonStyleEngine.InternalUpdateAttributes(
  const AStyle: TStyledButtonStyle;
  var ANormalStyle, ADownStyle, AFocusedStyle, AHotStyle,
  ADisabledStyle: TStyledButtonAttributes);
var
  LFontColor, LButtonColor: TColor;
  LOutLine: Boolean;
begin
  //Do not call inherited
  BootstrapClassToColors(AStyle, LFontColor, LButtonColor, LOutLine);

  //Default Style Attributes for Bootstrap Buttons
  ANormalStyle.BorderType := btRounded;
  ANormalStyle.FontStyle := [fsBold];
  ANormalStyle.FontName := 'Tahoma';
  ANormalStyle.OutLine := LOutLine;

  //Style for Normal Style of Bootstrap Button
  if LOutLine then
  begin
    //Outline: Border and FontColor same as Button Color
    ANormalStyle.BorderStyle := psSolid;
    ANormalStyle.FontColor := LButtonColor;
    ANormalStyle.BorderColor := LButtonColor;
  end
  else
  begin
    ANormalStyle.BorderStyle := psClear;
    ANormalStyle.FontColor := LFontColor;
    ANormalStyle.ButtonColor := LButtonColor;
    ANormalStyle.BorderColor := ANormalStyle.ButtonColor;
  end;

  //Clone Normal Style to Other Styles
  AssignStyle(ANormalStyle, ADownStyle);
  AssignStyle(ANormalStyle, AFocusedStyle);
  AssignStyle(ANormalStyle, AHotStyle);
  AssignStyle(ANormalStyle, ADisabledStyle);

  if LOutline then
  begin
    //Button Down: color as Button Color
    with ADownStyle do
    begin
      ButtonColor := LButtonColor;
      BorderColor := LightenColor(LButtonColor, 50);
      BorderStyle := psSolid;
      BorderWidth := DEFAULT_DOWN_BORDER_WIDTH;
      FontColor := LFontColor;
      OutLine := False;
    end;

    //Button Hot: color as Button Color
    with AHotStyle do
    begin
      ButtonColor := LButtonColor;
      //BorderColor := LightenColor(LButtonColor, 50);
      BorderStyle := psClear;
      BorderWidth := DEFAULT_DOWN_BORDER_WIDTH;
      FontColor := LFontColor;
      OutLine := False;
    end;

    //Button Focused
    with AFocusedStyle do
    begin
      ButtonColor := DarkenColor(LButtonColor, 20);
      BorderStyle := psSolid;
      BorderWidth := DEFAULT_FOCUSED_BORDER_WIDTH;
      FontColor := LFontColor;
      OutLine := False;
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
    ADownStyle.BorderStyle := psSolid;
    ADownStyle.BorderWidth := DEFAULT_DOWN_BORDER_WIDTH;

    //Button Hot: color as Down but no Border
    AHotStyle.ButtonColor := ADownStyle.ButtonColor;

    //Button Focused
    AFocusedStyle.ButtonColor := DarkenColor(LButtonColor, 20);
    AFocusedStyle.BorderStyle := psSolid;
    AFocusedStyle.BorderWidth := DEFAULT_FOCUSED_BORDER_WIDTH;

    //Button Disabled
    ADisabledStyle.ButtonColor := LightenColor(ANormalStyle.ButtonColor, 70);//ColortoGrayscale(LButtonColor);
    ADisabledStyle.FontColor := LightenColor(LFontColor, 70);
  end;
end;

end.
