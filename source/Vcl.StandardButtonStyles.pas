{******************************************************************************}
{                                                                              }
{  Standard Button Family: implementation of "standard" Family                 }
{  attributes for StyledButton similar to VCL Styled Buttons                   }
{                                                                              }
{  Copyright (c) 2022-2024 (Ethea S.r.l.)                                      }
{  Author: Carlo Barazzetta                                                    }
{  Contributors: Ariel Montes, Lance Rasmussen                                 }
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
unit Vcl.StandardButtonStyles;

interface

uses
  Vcl.Graphics
  , System.UITypes
  , Vcl.ButtonStylesAttributes;

const
  DEFAULT_STYLEDRAWTYPE = btRoundRect;
  DEFAULT_CLASSIC_FAMILY = 'Classic';
  DEFAULT_WINDOWS_CLASS = 'Windows';
  DEFAULT_APPEARANCE = 'Normal';
  OUTLINE_APPEARANCE = 'Outline';
  DEFAULT_CURSOR = crHandPoint;
  STD_BORDER_WIDTH = 2;

Type
  TButtonStandardStyles = class(TInterfacedObject, IStyledButtonAttributes)
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
  end;

  //Class to register Theme attributes (like dark or light)
  TThemeType = (ttLight, ttDark);
  TThemeAttribute = class
    StyleName: String;
    ThemeType: TThemeType;
    ButtonColor: TColor;
    BorderColor: TColor;
    BorderHotColor: TColor;
    HotColor: TColor;
    FontColor: TColor;
    FontHotColor: TColor;
    BorderType: TStyledButtonDrawType;
    IDEStyle: Boolean;
  end;

//To add new styles used from your application that are not managed by default
procedure RegisterThemeAttributes(const AVCLStyleName: string;
  const AThemeType: TThemeType;
  const AFontColor: TColor;
  const AFontHotColor: TColor;
  const AButtonColor: TColor;
  const AHotColor: TColor;
  const ABorderColor: TColor;
  const ABorderHotColor: TColor;
  const ABorderType: TStyledButtonDrawType;
  const AIDEStyle: Boolean = False);

function GetStyleAttributes(const AStyleName: string;
  out AThemeAttribute: TThemeAttribute): Boolean;

implementation

uses
  Winapi.Windows
  , Vcl.StyledButton
  , Vcl.Controls
  , Vcl.StdCtrls
  , Vcl.Themes
  , System.SysUtils
  , System.Math
  , System.Generics.Collections;

var
  ButtonClasses: TButtonClasses;
  ButtonAppearances: TButtonAppearances;
  ThemeAttributes: TList<TThemeAttribute>;

function GetStyleAttributes(const AStyleName: string;
  out AThemeAttribute: TThemeAttribute): Boolean;
var
  LWindowsThemeAttr, LThemeAttribute: TThemeAttribute;
begin
  LWindowsThemeAttr := nil;
  for LThemeAttribute in ThemeAttributes do
  begin
    if LThemeAttribute.StyleName = DEFAULT_WINDOWS_CLASS then
      LWindowsThemeAttr := LThemeAttribute;
    if SameText(AStyleName, LThemeAttribute.StyleName) then
    begin
      AThemeAttribute := LThemeAttribute;
      Exit(True); //Theme Attributes found: Exit from function
    end;
  end;
  AThemeAttribute := LWindowsThemeAttr;
  if AThemeAttribute = nil then
  raise Exception.CreateFmt('Attributes for Style "%s" not found!'+sLineBreak+
    'please call RegThemeAttr in an initialization section to add your custom style',[AStyleName])
  else
    Result := True;
end;

{ TStyledButtonStdStyle }

function TButtonStandardStyles.ButtonFamilyName: string;
begin
  Result := DEFAULT_CLASSIC_FAMILY;
end;

function TButtonStandardStyles.GetButtonClasses: TButtonClasses;
var
  LStylesCount: Integer;
  LStyleName: string;
  LThemeAttribute: TThemeAttribute;
  I: Integer;
begin
  if length(ButtonClasses) = 0 then
  begin
    LStylesCount := ThemeAttributes.Count;
    SetLength(ButtonClasses,LStylesCount);
    I := 0;
    for LThemeAttribute in ThemeAttributes do
    begin
      LStyleName := LThemeAttribute.StyleName;
      ButtonClasses[I] := LStyleName;
      Inc(I);
    end;
  end;
  Result := ButtonClasses;
end;

procedure TButtonStandardStyles.GetStyleByModalResult(
  const AModalResult: System.UITypes.TModalResult;
  var AStyleClass: TStyledButtonClass;
  var AStyleAppearance: TStyledButtonAppearance);
begin
  AStyleClass := DEFAULT_WINDOWS_CLASS;

  //define Appearance based on AModalResult
  case AModalResult of
    mrNone     : begin AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrYes      : begin AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrNo       : begin AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrOk       : begin AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrCancel   : begin AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrAbort    : begin AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrRetry    : begin AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrIgnore   : begin AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrAll      : begin AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrNoToAll  : begin AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrYesToAll : begin AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrClose    : begin AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrTryAgain : begin AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrContinue : begin AStyleAppearance := DEFAULT_APPEARANCE; end;
    mrHelp     : begin AStyleAppearance := DEFAULT_APPEARANCE; end;
  else
    GetStyleByModalResult(mrNone, AStyleClass, AStyleAppearance);
  end;
end;

function TButtonStandardStyles.GetButtonAppearances: TButtonAppearances;
begin
  if length(ButtonAppearances) = 0 then
  begin
    SetLength(ButtonAppearances, 2);
    ButtonAppearances[0] := DEFAULT_APPEARANCE;
    ButtonAppearances[1] := OUTLINE_APPEARANCE;
  end;
  Result := ButtonAppearances;
end;

procedure StandardClassToColors(const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var AFontColor, AFontHotColor, AButtonColor, AHotColor, ABorderColor, ABorderHotColor: TColor;
  var ABorderType: TStyledButtonDrawType;
  out AOutLine, ADarkStyle: Boolean);
var
  LStyleName: string;
  LThemeAttribute: TThemeAttribute;
begin
  //Default for Windows
  ADarkStyle := False;
  AButtonColor := clBtnFace;
  ABorderColor := clBtnShadow;
  AFontColor := clWindowText;

  //mapping VCL Button Styles colors and Dark/Light Style
  LStyleName := AClass;
  GetStyleAttributes(LStyleName, LThemeAttribute);
  AButtonColor    := LThemeAttribute.ButtonColor;
  AHotColor       := LThemeAttribute.HotColor;
  ABorderColor    := LThemeAttribute.BorderColor;
  ABorderHotColor := LThemeAttribute.BorderHotColor;
  ADarkStyle      := LThemeAttribute.ThemeType = ttDark;
  ABorderType     := LThemeAttribute.BorderType;
  AFontColor      := LThemeAttribute.FontColor;
  AFontHotColor   := LThemeAttribute.FontHotColor;

  AOutLine := SameText(AAppearance, OUTLINE_APPEARANCE);
end;

procedure TButtonStandardStyles.UpdateAttributes(
  const AFamily:  TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  var ANormalStyle, APressedStyle, ASelectedStyle, AHotStyle,
  ADisabledStyle: TStyledButtonAttributes);
var
  LFontColor, LFontHotColor, LButtonColor, LHotColor, LBorderColor, LBorderHotColor: TColor;
  LBorderType: TStyledButtonDrawType;
  LOutLine: Boolean;
  LDarkStyle: Boolean;
begin
  StandardClassToColors(AClass, AAppearance, LFontColor, LFontHotColor,
    LButtonColor, LHotColor, LBorderColor, LBorderHotColor, LBorderType, LOutLine, LDarkStyle);

  //Default Style Attributes for Classic Buttons
  ANormalStyle.DrawType := LBorderType;

  //Outline: Invert Border Color and Button Color
  if LOutline then
  begin
    //Outline: Border and FontColor same as Button Color
    ANormalStyle.ButtonDrawStyle := btnClear;
    ANormalStyle.BorderDrawStyle := brdSolid;
    ANormalStyle.FontStyle := [fsBold];
    ANormalStyle.FontColor := LButtonColor;
    ANormalStyle.ButtonColor := LBorderColor;
    ANormalStyle.BorderColor := LButtonColor;
    ANormalStyle.BorderWidth := STD_BORDER_WIDTH + 1;
  end
  else
  begin
    ANormalStyle.ButtonDrawStyle := btnSolid;
    ANormalStyle.BorderDrawStyle := brdSolid;
    ANormalStyle.FontStyle := [];
    ANormalStyle.FontColor := LFontColor;
    ANormalStyle.ButtonColor := LButtonColor;
    ANormalStyle.BorderColor := LBorderColor;
    ANormalStyle.BorderWidth := STD_BORDER_WIDTH;
  end;

  //Clone Normal Style to Other Styles, except Selected
  CloneButtonStyle(ANormalStyle, APressedStyle);
  CloneButtonStyle(ANormalStyle, ASelectedStyle);
  CloneButtonStyle(ANormalStyle, AHotStyle);
  CloneButtonStyle(ANormalStyle, ADisabledStyle);

  //Hot Button
  AHotStyle.ButtonColor := LHotColor;
  AHotStyle.BorderColor := LBorderHotColor;
  AHotStyle.FontColor   := LFontHotColor;

  //Selected Button (focused), similar to Hot
  //with lighter or darker button color
  CloneButtonStyle(AHotStyle, ASelectedStyle);
  if LDarkStyle then
    ASelectedStyle.ButtonColor := LightenColor(LHotColor, 10)
  else
    ASelectedStyle.ButtonColor := DarkenColor(LHotColor, 10);

  //Pressed Button, , similar to Hot
  //with lighter or darker button and border color
  CloneButtonStyle(AHotStyle, APressedStyle);
  if LDarkStyle then
  begin
    APressedStyle.ButtonColor := LightenColor(LButtonColor, 20);
    APressedStyle.BorderColor := DarkenColor(LBorderColor, 20);
  end
  else
  begin
    APressedStyle.ButtonColor := DarkenColor(LHotColor, 20);
    APressedStyle.BorderColor := LightenColor(LHotColor, 20);
  end;

  //Disabled Button (lighten)
  if LDarkStyle then
  begin
    ADisabledStyle.BorderColor := LightenColor(ADisabledStyle.BorderColor, 10);
    ADisabledStyle.ButtonColor := LightenColor(ADisabledStyle.ButtonColor, 10);
    ADisabledStyle.FontColor := DarkenColor(ADisabledStyle.FontColor, 50);
  end
  else
  begin
    ADisabledStyle.BorderColor := LightenColor(ADisabledStyle.BorderColor, 50);
    ADisabledStyle.ButtonColor := LightenColor(ADisabledStyle.ButtonColor, 50);
    ADisabledStyle.FontColor := LightenColor(ADisabledStyle.FontColor, 50);
  end;
end;

procedure RegisterThemeAttributes(
  const AVCLStyleName: string;
  const AThemeType: TThemeType;
  const AFontColor: TColor;
  const AFontHotColor: TColor;
  const AButtonColor: TColor;
  const AHotColor: TColor;
  const ABorderColor: TColor;
  const ABorderHotColor: TColor;
  const ABorderType: TStyledButtonDrawType;
  const AIDEStyle: Boolean = False);
var
  LThemeAttribute: TThemeAttribute;

  procedure UpdateThemeAttributes;
  begin
    LThemeAttribute.StyleName      := AVCLStyleName;
    LThemeAttribute.ThemeType      := AThemeType;
    LThemeAttribute.ButtonColor    := AButtonColor;
    LThemeAttribute.BorderColor    := ABorderColor;
    LThemeAttribute.BorderHotColor := ABorderHotColor;
    LThemeAttribute.FontColor      := AFontColor;
    LThemeAttribute.FontHotColor   := AFontHotColor;
    LThemeAttribute.HotColor       := AHotColor;
    LThemeAttribute.BorderType     := ABorderType;
    LThemeAttribute.IDEStyle       := AIDEStyle;
  end;

begin
  for LThemeAttribute in ThemeAttributes do
  begin
    if SameText(LThemeAttribute.StyleName, AVCLStyleName) then
    begin
      UpdateThemeAttributes;
      Exit; //Found: exit
    end;
  end;
  //not found
  LThemeAttribute := TThemeAttribute.Create;
  ThemeAttributes.Add(LThemeAttribute);
  UpdateThemeAttributes;
end;

procedure InitDefaultThemesAttributes;
var
  LButtonBorder: TStyledButtonDrawType;

  function IsWindows11: Boolean;
  begin
    Result := GetWindowsVersion = wvWindows11;
  end;

begin
  ThemeAttributes := TList<TThemeAttribute>.Create;

  if IsWindows11 then
    LButtonBorder := btRoundRect
  else
    LButtonBorder := btRect;

  //Non themed Windows Style
  RegisterThemeAttributes('Windows',ttLight,clBlack,clBlack,
    htmlToColor('#fdfdfd'),htmlToColor('#e0eef9'),
    htmlToColor('#d0d0d0'),htmlToColor('#0078d4'),LButtonBorder);

  if StyleServices.Enabled then
  begin
    RegisterThemeAttributes('Amakrits',ttDark,clWhite,clBlack,
      htmlToColor('#292929'),htmlToColor('#6EE4D3'),
      htmlToColor('#020202'),htmlToColor('#020202'),btRect);
    RegisterThemeAttributes('Amethyst Kamri',ttLight,clBlack,clBlack,
      htmlToColor('#cdd1e2'),htmlToColor('#F99369'),
      htmlToColor('#868db0'),htmlToColor('#868db0'),btRoundRect);
    RegisterThemeAttributes('Aqua Graphite',ttDark,clWhite,clWhite,
      htmlToColor('#0070bb'),htmlToColor('#1585cc'),
      htmlToColor('#043f5c'),htmlToColor('#043f5c'),btRoundRect);
    RegisterThemeAttributes('Aqua Light Slate',ttLight,clBlack,clBlack,
      htmlToColor('#f1f1f1'),htmlToColor('#a9d8f2'),
      htmlToColor('#9c9c9c'),htmlToColor('#9c9c9c'),btRoundRect);
    RegisterThemeAttributes('Auric'	,ttDark,clWhite,clBlack,
      htmlToColor('#5e5e5f'),htmlToColor('#fad535'),
      clBlack,clBlack,btRoundRect);
    RegisterThemeAttributes('Calypso',ttDark,clWhite,clBlack,
      htmlToColor('#44617c'),htmlToColor('#80ceb5'),
      htmlToColor('#44617c'),htmlToColor('#80ceb5'),btRoundRect);
    RegisterThemeAttributes('Calypso LE',ttDark,clWhite,clBlack,
      htmlToColor('#44617c'),htmlToColor('#80ceb5'),
      htmlToColor('#44617c'),htmlToColor('#80ceb5'),btRoundRect);
    RegisterThemeAttributes('Calypso SE',ttDark,clWhite,clBlack,
      htmlToColor('#44617c'),htmlToColor('#6bafe2'),
      htmlToColor('#44617c'),htmlToColor('#6bafe2'),btRoundRect);
    RegisterThemeAttributes('Calypso SLE',ttDark,clWhite,clBlack,
      htmlToColor('#44617c'),htmlToColor('#6bafe2'),
      htmlToColor('#44617c'),htmlToColor('#6bafe2'),btRoundRect);
    RegisterThemeAttributes('Carbon',ttDark,htmlToColor('#c0c0c0'),clWhite,
      htmlToColor('#3a3a3a'),htmlToColor('#3e3e3e'),
      htmlToColor('#191919'),htmlToColor('#2E6176'),btRect);
    RegisterThemeAttributes('Charcoal Dark Slate',ttDark,htmlToColor('#a3a3a3'),clWhite,
      htmlToColor('#3a3a3a'),htmlToColor('#1f1f1f'),
      htmlToColor('#191919'),htmlToColor('#191919'),btRoundRect);
    RegisterThemeAttributes('Cobalt XEMedia',ttDark,htmlToColor('#c0c0c0'),clWhite,
      htmlToColor('#121e32'),htmlToColor('#0f2c54'),
      htmlToColor('#a1a5ab'),htmlToColor('#a1a5ab'),btRoundRect);
    RegisterThemeAttributes('Copper',ttLight,clBlack,clWhite,
      clWhite,htmlToColor('#e46b60'),
      clWhite,htmlToColor('#e46b60'),btRoundRect);
    RegisterThemeAttributes('CopperDark',ttDark,clWhite,clWhite,
      htmlToColor('#2b2b2b'),htmlToColor('#e46b60'),
      htmlToColor('#898989'),htmlToColor('#E46B60'),btRoundRect);
    RegisterThemeAttributes('Coral',ttDark,clWhite,clWhite,
      htmlToColor('#d86d00'),htmlToColor('#d86d00'),
      htmlToColor('#d86d00'),htmlToColor('#d1d1d1'),btRoundRect);
    RegisterThemeAttributes('Cyan Dusk',ttLight,clBlack,clWhite,
      htmlToColor('#b4bac2'),htmlToColor('#324a65'),
      htmlToColor('#687e97'),htmlToColor('#687e97'),btRoundRect);
    RegisterThemeAttributes('Cyan Night',ttLight,clBlack,clWhite,
      htmlToColor('#b0b4bf'),htmlToColor('#323c62'),
      htmlToColor('#687297'),htmlToColor('#687297'),btRoundRect);
    RegisterThemeAttributes('Diamond',ttLight,clBlack,clBlack,
      htmlToColor('#efeff0'),htmlToColor('#f5f5f5'),
      htmlToColor('#c4c3c3'),htmlToColor('#c4c3c3'),btRoundRect);
    RegisterThemeAttributes('Emerald',ttDark,clWhite,clWhite,
      htmlToColor('#00a57d'),htmlToColor('#00a57d'),
      htmlToColor('#00a57d'),htmlToColor('#cbcbc7'),btRoundRect);
    RegisterThemeAttributes('Emerald Light Slate',ttLight,clBlack,clBlack,
      htmlToColor('#e0e0e0'),htmlToColor('#87e168'),
      htmlToColor('#9c9c9c'),htmlToColor('#9c9c9c'),btRoundRect);
    RegisterThemeAttributes('Flat UI Light',ttLight,clBlack,clWhite,
      htmlToColor('#e4e6e7'),htmlToColor('#3498da'),
      htmlToColor('#e4e6e7'),htmlToColor('#3498da'),btRoundRect);
    RegisterThemeAttributes('Glossy',ttDark,clWhite,clWhite,
      htmlToColor('#3d3d3d'),htmlToColor('#2f65a7'),
      clBlack,clBlack,btRoundRect);
    RegisterThemeAttributes('Glow',ttDark,clWhite,htmlToColor('#37bdbb'),
      htmlToColor('#2e343c'),htmlToColor('#2d333b'),
      htmlToColor('#1a1c1f'),htmlToColor('#1a1c1f'),btRoundRect);
    RegisterThemeAttributes('Golden Graphite',ttDark,htmlToColor('#eaeaea'),clWhite,
      htmlToColor('#bb8900'),htmlToColor('#c6920f'),
      htmlToColor('#5c3e04'),htmlToColor('#5c3e04'),btRoundRect);
    RegisterThemeAttributes('Iceberg Classico',ttLight,clBlack,clBlack,
      htmlToColor('#e4eaf1'),htmlToColor('#99c7ea'),
      htmlToColor('#91a6c0'),htmlToColor('#91a6c0'),btRoundRect);
    RegisterThemeAttributes('Jet',ttDark,clWhite,htmlToColor('#cee9f8'),
      htmlToColor('#303030'),htmlToColor('#393939'),
      htmlToColor('#585858'),htmlToColor('#585858'),btRoundRect);
    RegisterThemeAttributes('Lavender Classico',ttLight,clBlack,clBlack,
      htmlToColor('#eaeaee'),htmlToColor('#a9c4e6'),
      htmlToColor('#a1a1b4'),htmlToColor('#a1a1b4'),btRoundRect);
    RegisterThemeAttributes('Light',ttLight,clBlack,clBlack,
      htmlToColor('#fdfdfd'),htmlToColor('#d4e6f3'),
      htmlToColor('#ababab'),htmlToColor('#a2c7e4'),btRect);
    RegisterThemeAttributes('Lucky Point',ttDark,clWhite,clBlack,
      htmlToColor('#3f4c6a'),htmlToColor('#74b9c9'),
      htmlToColor('#3f4c6a'),htmlToColor('#74b9c9'),btRoundRect);
    RegisterThemeAttributes('Luna',ttLight,clBlack,clBlack,
      htmlToColor('#bcd0e9'),htmlToColor('#ffd355'),
      htmlToColor('#99b5de'),htmlToColor('#99b5de'),btRoundRect);
    RegisterThemeAttributes('Material Oxford Blue',ttDark,clWhite,clWhite,
      htmlToColor('#5f6a72'),htmlToColor('#00a1a1'),
      htmlToColor('#5f6a72'),htmlToColor('#00a1a1'),btRoundRect);
    RegisterThemeAttributes('Material Oxford Blue SE',ttDark,clWhite,clWhite,
      htmlToColor('#5f6a72'),htmlToColor('#0a7fbf'),
      htmlToColor('#5f6a72'),htmlToColor('#0a7fbf'),btRoundRect);
    RegisterThemeAttributes('Material Patterns Blue',ttLight,clBlack,clBlack,
      htmlToColor('#e6ecf2'),htmlToColor('#e6ecf2'),
      htmlToColor('#c4d3df'),htmlToColor('#18a1e9'),btRoundRect);
    RegisterThemeAttributes('Metropolis UI Black',ttDark,clWhite,clWhite,
      htmlToColor('#1A1A1A'),htmlToColor('#1373A9'),
      htmlToColor('#F8F8F8'),htmlToColor('#F8F8F8'),btRoundRect);
    RegisterThemeAttributes('Metropolis UI Blue',ttDark,clWhite,clWhite,
      htmlToColor('#1373A9'),htmlToColor('#E86625'),
      htmlToColor('#F8F8F8'),htmlToColor('#F8F8F8'),btRoundRect);
    RegisterThemeAttributes('Metropolis UI Dark',ttDark,clWhite,clWhite,
      htmlToColor('#1A1A1A'),htmlToColor('#5E5E5E'),
      htmlToColor('#F8F8F8'),htmlToColor('#F8F8F8'),btRect);
    RegisterThemeAttributes('Metropolis UI Green',ttDark,clWhite,clWhite,
      htmlToColor('#096C37'),htmlToColor('#247C4D'),
      clWhite,clWhite,btRect);
    RegisterThemeAttributes('Obsidian',ttDark,clBlack,clBlack,
      htmlToColor('#cfd2d7'),htmlToColor('#ffd24e'),
      htmlToColor('#2f2f2f'),htmlToColor('#2f2f2f'),btRoundRect);
    RegisterThemeAttributes('Onyx Blue',ttLight,clBlack,clWhite,
      htmlToColor('#adb0b4'),htmlToColor('#42729a'),
      htmlToColor('#adb0b4'),htmlToColor('#42729a'),btRoundRect);
    RegisterThemeAttributes('Puerto Rico',ttDark,clWhite,clWhite,
      htmlToColor('#44beb0'),htmlToColor('#52cfc0'),
      htmlToColor('#44beb0'),htmlToColor('#52cfc0'),btRoundRect);
    RegisterThemeAttributes('Radiant',ttDark,clWhite,clWhite,
      htmlToColor('#00b8b0'),htmlToColor('#19bfb7'),
      htmlToColor('#00b8b0'),htmlToColor('#19bfb7'),btRoundRect);
    RegisterThemeAttributes('Ruby Graphite',ttDark,clWhite,clWhite,
      htmlToColor('#bb0d00'),htmlToColor('#cc1e15'),
      htmlToColor('#5c0404'),htmlToColor('#5c0404'),btRoundRect);
    RegisterThemeAttributes('Sapphire Kamri',ttLight,clBlack,clBlack,
      htmlToColor('#c1d9e7'),htmlToColor('#fa946b'),
      htmlToColor('#6d9dbf'),htmlToColor('#a57b6c'),btRoundRect);
    RegisterThemeAttributes('Silver',ttLight,clBlack,clBlack,
      htmlToColor('#dce0e6'),htmlToColor('#ffd24e'),
      htmlToColor('#b9bec8'),htmlToColor('#b9bec8'),btRoundRect);
    RegisterThemeAttributes('Sky',ttLight,clBlack,clBlack,
      htmlToColor('#efefef'),htmlToColor('#acdbef'),
      htmlToColor('#bebebe'),htmlToColor('#bebebe'),btRoundRect);
    RegisterThemeAttributes('Slate Classico',ttLight,clBlack,clBlack,
      htmlToColor('#ebebeb'),htmlToColor('#99c7ea'),
      htmlToColor('#a8a8a8'),htmlToColor('#6692be'),btRoundRect);
    RegisterThemeAttributes('Smokey Quartz Kamri',ttLight,clBlack,clBlack,
      htmlToColor('#dbdbdb'),htmlToColor('#f39772'),
      htmlToColor('#9f9f9f'),htmlToColor('#9f9f9f'),btRoundRect);
    RegisterThemeAttributes('Stellar',ttLight,htmlToColor('#3e629a'),htmlToColor('#3e629a'),
      htmlToColor('#fcfcfc'),clWhite,
      htmlToColor('#fcfcfc'),htmlToColor('#fcfcfc'),btRoundRect);
    RegisterThemeAttributes('Stellar Dark',ttLight,htmlToColor('#3e629a'),htmlToColor('#3e629a'),
      htmlToColor('#fcfcfc'),clWhite,
      htmlToColor('#fcfcfc'),htmlToColor('#fcfcfc'),btRoundRect);
    RegisterThemeAttributes('Sterling',ttLight,clBlack,htmlToColor('#527593'),
      htmlToColor('#f9fafc'),htmlToColor('#eef6fb'),
      htmlToColor('#dde1e4'),htmlToColor('#dde1e4'),btRoundRect);
    RegisterThemeAttributes('Tablet Dark',ttDark,clWhite,clBlack,
      htmlToColor('#3d4a79'),htmlToColor('#1abc9c'),
      htmlToColor('#3d4a79'),htmlToColor('#1abc9c'),btRoundRect);
    RegisterThemeAttributes('TabletDark',ttDark,clWhite,clBlack,
      htmlToColor('#3d4a79'),htmlToColor('#1abc9c'),
      htmlToColor('#3d4a79'),htmlToColor('#1abc9c'),btRoundRect);
    RegisterThemeAttributes('Tablet Light',ttLight,clBlack,clWhite,
      clWhite,htmlToColor('#3d84dd'),
      htmlToColor('#3d84dd'),htmlToColor('#3d84dd'),btRoundRect);
    RegisterThemeAttributes('Turquoise Gray',ttLight,clBlack,clWhite,
      htmlToColor('#ededed'),htmlToColor('#28c0e9'),
      htmlToColor('#d3d3d3'),htmlToColor('#019ac4'),btRoundRect);
    RegisterThemeAttributes('Vapor',ttLight,clBlack,clBlack,
      htmlToColor('#89dcc8'),htmlToColor('#7cc6b4'),
      htmlToColor('#89dcc8'),htmlToColor('#7cc6b4'),btRoundRect);
    RegisterThemeAttributes('Wedgewood Light',ttLight,clBlack,clWhite,
      htmlToColor('#f5f5f6'),htmlToColor('#5a7390'),
      htmlToColor('#dfdfe1'),htmlToColor('#dfdfe1'),btRoundRect);
    RegisterThemeAttributes('Windows10',ttLight,clBlack,clBlack,
      htmlToColor('#cccccc'),htmlToColor('#cccccc'),
      htmlToColor('#cccccc'),htmlToColor('#7a7a7a'),btRect);
    RegisterThemeAttributes('Windows10 BlackPearl',ttDark,clWhite,clWhite,
      htmlToColor('#36424a'),htmlToColor('#36424a'),
      htmlToColor('#36424a'),htmlToColor('#f36638'),btRect);
    RegisterThemeAttributes('Windows10 Blue',ttDark,clWhite,clWhite,
      htmlToColor('#084f8a'),htmlToColor('#084f8a'),
      htmlToColor('#084f8a'),clWhite,btRect);
    RegisterThemeAttributes('Windows10 Blue Whale',ttDark,clWhite,clWhite,
      htmlToColor('#364762'),htmlToColor('#364762'),
      htmlToColor('#364762'),htmlToColor('#0c96f2'),btRect);
    RegisterThemeAttributes('Windows10 Blue Whale LE',ttDark,clWhite,clWhite,
      htmlToColor('#364762'),htmlToColor('#364762'),
      htmlToColor('#364762'),htmlToColor('#0c96f2'),btRect);
    RegisterThemeAttributes('Windows10 Charcoal',ttDark,clWhite,clWhite,
      htmlToColor('#303030'),htmlToColor('#2a2a2a'),
      htmlToColor('#545454'),htmlToColor('#1c68a8'),btRect);
    RegisterThemeAttributes('Windows10 Clear Day',ttLight,clBlack,clBlack,
      htmlToColor('#e5f0f6'),htmlToColor('#aed8f8'),
      htmlToColor('#aed9f8'),htmlToColor('#67baed'),btRect);
    RegisterThemeAttributes('Windows10 Dark',ttDark,clWhite,clWhite,
      htmlToColor('#262626'),htmlToColor('#262626'),
      htmlToColor('#262626'),htmlToColor('#7e7e7e'),btRect);
    RegisterThemeAttributes('Windows10 Green',ttDark,clWhite,clWhite,
      htmlToColor('#055249'),htmlToColor('#055249'),
      htmlToColor('#055249'),clWhite,btRect);
    RegisterThemeAttributes('Windows10 Malibu',ttLight,clBlack,clWhite,
      htmlToColor('#deecf0'),htmlToColor('#64c6da'),
      htmlToColor('#d8e5e9'),htmlToColor('#64c6da'),btRect);
    RegisterThemeAttributes('Windows10 Purple',ttDark,clWhite,clWhite,
      htmlToColor('#672d63'),htmlToColor('#672d63'),
      htmlToColor('#672d63'),clWhite,btRect);
    RegisterThemeAttributes('Windows10 SlateGray',ttDark,clWhite,
      htmlToColor('#7daca8'),htmlToColor('#2a353b'),
      htmlToColor('#2a353b'),htmlToColor('#2a353b'),
      htmlToColor('#7daca8'),btRect);
    RegisterThemeAttributes('Windows11 MineShaft',ttDark,clWhite,clBlack,
      htmlToColor('#373737'),htmlToColor('#47b1e8'),
      htmlToColor('#373737'),htmlToColor('#47b1e8'),btRoundRect);
    RegisterThemeAttributes('Windows11 Modern Dark',ttDark,clWhite,clWhite,
      htmlToColor('#373737'),htmlToColor('#405560'),
      htmlToColor('#434343'),htmlToColor('#4ab2e9'),btRoundRect);
    RegisterThemeAttributes('Windows11 Modern Light',ttLight,clBlack,clBlack,
      htmlToColor('#fdfdfd'),htmlToColor('#eef4f9'),
      htmlToColor('#bbbbbb'),htmlToColor('#0067c0'),btRoundRect);
    RegisterThemeAttributes('Windows11 Polar Dark',ttDark,clWhite,clWhite,
      htmlToColor('#4b5167'),htmlToColor('#0781e0'),
      htmlToColor('#4b5167'),htmlToColor('#0781e0'),btRoundRect);
    RegisterThemeAttributes('Windows11 Polar Light',ttLight,clBlack,clBlack,
      htmlToColor('#c7d4e1'),htmlToColor('#a2d0fe'),
      htmlToColor('#c7d4e1'),htmlToColor('#a2d0fe'),btRoundRect);
    RegisterThemeAttributes('Windows11 White Smoke',ttLight,clBlack,clWhite,
      htmlToColor('#fdfdfd'),htmlToColor('#1975c5'),
      htmlToColor('#e9e9e9'),htmlToColor('#1975c5'),btRoundRect);
    RegisterThemeAttributes('Zircon',ttLight,clBlack,clBlack,
      htmlToColor('#e5e8e9'),htmlToColor('#a0d4de'),
      htmlToColor('#e5e8e9'),htmlToColor('#a0d4de'),btRoundRect);
    RegisterThemeAttributes('Zircon SE',ttLight,clBlack,clBlack,
      htmlToColor('#e5e8e9'),htmlToColor('#a0d4de'),
      htmlToColor('#e5e8e9'),htmlToColor('#a0d4de'),btRoundRect);
    RegisterThemeAttributes('Windows11 Impressive Dark',ttDark,clWhite,clWhite,
      htmlToColor('#3F506D'),htmlToColor('#5172EF'),
      htmlToColor('#3F506D'),htmlToColor('#5172EF'),btRoundRect);
    RegisterThemeAttributes('Windows11 Impressive Dark SE',ttDark,clWhite,clWhite,
      htmlToColor('#3F506D'),htmlToColor('#5172EF'),
      htmlToColor('#3F506D'),htmlToColor('#5172EF'),btRoundRect);
    RegisterThemeAttributes('Windows11 Impressive Light',ttLight,clBlack,clWhite,
      htmlToColor('#CAD4E6'),htmlToColor('#5172EF'),
      htmlToColor('#CAD4E6'),htmlToColor('#5172EF'),btRoundRect);
    RegisterThemeAttributes('Windows11 Impressive Light SE',ttLight,clBlack,clWhite,
      htmlToColor('#CAD4E6'),htmlToColor('#5172EF'),
      htmlToColor('#CAD4E6'),htmlToColor('#5172EF'),btRoundRect);

    //New Third Parties Styles (provided by Lance Rasmussen)
    RegisterThemeAttributes('CDE Frost: Blue',ttDark,clWhite,clWhite,
      htmlToColor('#084f8a'),htmlToColor('#084f8a'),
      htmlToColor('#084f8a'),clWhite,btRect);
    RegisterThemeAttributes('CDE Frost: Teal',ttDark,clWhite,clWhite,
      htmlToColor('#3F506D'),htmlToColor('#5172EF'),
      htmlToColor('#3F506D'),htmlToColor('#5172EF'),btRoundRect);
    RegisterThemeAttributes('CDE: Golden Graphite',ttDark,clWhite,clWhite,
      htmlToColor('#3F506D'),htmlToColor('#5172EF'),
      htmlToColor('#3F506D'),htmlToColor('#5172EF'),btRoundRect);
    RegisterThemeAttributes('CDE Graphite: Blue',ttDark,clWhite,clWhite,
      htmlToColor('#084f8a'),htmlToColor('#084f8a'),
      htmlToColor('#084f8a'),clWhite,btRect);
    RegisterThemeAttributes('CDE Graphite: Teal',ttDark,clWhite,clWhite,
      htmlToColor('#3F506D'),htmlToColor('#5172EF'),
      htmlToColor('#3F506D'),htmlToColor('#5172EF'),btRoundRect);
    RegisterThemeAttributes('CDE Impressive: Dark',ttDark,clWhite,clWhite,
      htmlToColor('#3F506D'),htmlToColor('#5172EF'),
      htmlToColor('#3F506D'),htmlToColor('#5172EF'),btRoundRect);
    RegisterThemeAttributes('CDE Impressive: Light',ttLight,clBlack,clWhite,
      htmlToColor('#CAD4E6'),htmlToColor('#5172EF'),
      htmlToColor('#CAD4E6'),htmlToColor('#5172EF'),btRoundRect);
     RegisterThemeAttributes('CDE Modern: Dark',ttDark,clWhite,clWhite,
      htmlToColor('#3F506D'),htmlToColor('#5172EF'),
      htmlToColor('#3F506D'),htmlToColor('#5172EF'),btRoundRect);
    RegisterThemeAttributes('CDE Modern: Light',ttLight,clBlack,clWhite,
      htmlToColor('#CAD4E6'),htmlToColor('#5172EF'),
      htmlToColor('#CAD4E6'),htmlToColor('#5172EF'),btRoundRect);
    RegisterThemeAttributes('Raize Frost : Blue',ttDark,clWhite,clWhite,
      htmlToColor('#084f8a'),htmlToColor('#084f8a'),
      htmlToColor('#084f8a'),clWhite,btRect);
    RegisterThemeAttributes('Raize Frost : Green',ttDark,clWhite,clWhite,
      htmlToColor('#084f8a'),htmlToColor('#084f8a'),
      htmlToColor('#084f8a'),clWhite,btRect);
    RegisterThemeAttributes('Raize Frost : Orange',ttDark,clWhite,clWhite,
      htmlToColor('#084f8a'),htmlToColor('#084f8a'),
      htmlToColor('#084f8a'),clWhite,btRect);
    RegisterThemeAttributes('Raize Frost : Purple',ttDark,clWhite,clWhite,
      htmlToColor('#084f8a'),htmlToColor('#084f8a'),
      htmlToColor('#084f8a'),clWhite,btRect);
    RegisterThemeAttributes('Raize Frost : Red',ttDark,clWhite,clWhite,
      htmlToColor('#084f8a'),htmlToColor('#084f8a'),
      htmlToColor('#084f8a'),clWhite,btRect);
    RegisterThemeAttributes('Raize Frost : Teal',ttDark,clWhite,clWhite,
      htmlToColor('#084f8a'),htmlToColor('#084f8a'),
      htmlToColor('#084f8a'),clWhite,btRect);
    RegisterThemeAttributes('Raize Frost : Yellow',ttDark,clWhite,clWhite,
      htmlToColor('#084f8a'),htmlToColor('#084f8a'),
      htmlToColor('#084f8a'),clWhite,btRect);
    RegisterThemeAttributes('Raize Graphite : Blue',ttDark,clWhite,clWhite,
      htmlToColor('#084f8a'),htmlToColor('#084f8a'),
      htmlToColor('#084f8a'),clWhite,btRect);
    RegisterThemeAttributes('Raize Graphite : Green',ttDark,clWhite,clWhite,
      htmlToColor('#084f8a'),htmlToColor('#084f8a'),
      htmlToColor('#084f8a'),clWhite,btRect);
    RegisterThemeAttributes('Raize Graphite : Orange',ttDark,clWhite,clWhite,
      htmlToColor('#084f8a'),htmlToColor('#084f8a'),
      htmlToColor('#084f8a'),clWhite,btRect);
    RegisterThemeAttributes('Raize Graphite : Purple',ttDark,clWhite,clWhite,
      htmlToColor('#084f8a'),htmlToColor('#084f8a'),
      htmlToColor('#084f8a'),clWhite,btRect);
    RegisterThemeAttributes('Raize Graphite : Red',ttDark,clWhite,clWhite,
      htmlToColor('#084f8a'),htmlToColor('#084f8a'),
      htmlToColor('#084f8a'),clWhite,btRect);
    RegisterThemeAttributes('Raize Graphite : Teal',ttDark,clWhite,clWhite,
      htmlToColor('#084f8a'),htmlToColor('#084f8a'),
      htmlToColor('#084f8a'),clWhite,btRect);
    RegisterThemeAttributes('Raize Graphite : Yellow',ttDark,clWhite,clWhite,
      htmlToColor('#084f8a'),htmlToColor('#084f8a'),
      htmlToColor('#084f8a'),clWhite,btRect);
  end;

  //IDE Styled
  RegisterThemeAttributes('Win10IDE_Dark',ttDark,clWhite,clWhite,
    htmlToColor('#373737'),htmlToColor('#405560'),
    htmlToColor('#434343'),htmlToColor('#4ab2e9'),btRoundRect, True);
  RegisterThemeAttributes('Win10IDE_Light',ttLight,clBlack,clBlack,
    htmlToColor('#fdfdfd'),htmlToColor('#eef4f9'),
    htmlToColor('#bbbbbb'),htmlToColor('#0067c0'),btRoundRect, True);
  RegisterThemeAttributes('Mountain_Mist',ttLight,clBlack,clBlack,
    htmlToColor('#fdfdfd'),htmlToColor('#eef4f9'),
    htmlToColor('#bbbbbb'),htmlToColor('#0067c0'),btRoundRect, True);
end;

procedure FreeThemesAttributes;
var
  LThemeAttribute: TThemeAttribute;
begin
  for LThemeAttribute in ThemeAttributes do
    LThemeAttribute.Free;
  ThemeAttributes.Free;
end;

initialization
  SetLength(ButtonClasses,0);
  SetLength(ButtonAppearances,0);

  InitDefaultThemesAttributes;

  RegisterButtonFamily(
    TButtonStandardStyles.Create);

finalization
  FreeThemesAttributes;

end.
