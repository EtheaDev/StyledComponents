{******************************************************************************}
{                                                                              }
{  TStyledButtonAttributes: a collection of Rendering attributes               }
{  for Styled Components                                                       }
{  TNotificationBadgeAttributes: a set of Rendering attributes for Badge       }
{                                                                              }
{  Copyright (c) 2022-2024 (Ethea S.r.l.)                                      }
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
unit Vcl.ButtonStylesAttributes;

interface

{$INCLUDE StyledComponents.inc}
{$IFDEF D10_4+}
  {$R CommandLinkPNG.RES}
{$ELSE}
  {$R CommandLinkBMP.RES}
{$ENDIF}

uses
  System.UITypes
  , System.SysUtils
  , System.Classes
  , System.Contnrs
  , System.Types
  , Winapi.Windows
  , Winapi.CommCtrl
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.Buttons
  , Vcl.StdCtrls
  , Vcl.ImgList
  , Vcl.Themes
  ;

const
  DEFAULT_RADIUS = 6;
  RESOURCE_SHIELD_ICON = 'BUTTON_SHIELD_ADMIN';
  DEFAULT_MAX_BADGE_VALUE = 99;
  DEFAULT_BADGE_COLOR = clRed;
  DEFAULT_BADGE_FONT_COLOR = clWhite;
  DEFAULT_AUTOCLICK_DELAY = 5000; //Five Seconds

resourcestring
  ERROR_FAMILY_NOT_FOUND = 'Styled Button Family "%s" not found';
  ERROR_NEGATIVE_VALUE = 'Error: Notification Count cannot be negative!';
  ERROR_VALUE_OUT_OF_RANGE = 'Error: Value "%d" for "%s" is out of Range (%d-%d)';

Type
  EStyledAttributesException = class(Exception);

  //Windows Version
  TWindowsVersion = (wvUndefined, wvWindowsXP, wvWindowsVista, wvWindows7,
    wvWindows8, wvWindows8_1, wvWindows10, wvWindows11);

  TNotificationBadgePosition = (nbpTopRight, nbpTopLeft, nbpBottomRight, nbpBottomLeft);
  TNotificationBadgeSize = (nbsNormal, nbsSmallDot);

  //string typed attributes
  TStyledButtonFamily = string;
  TStyledButtonClass = string;
  TStyledButtonAppearance = string;

  //Type of border
  TStyledButtonDrawType = (btRoundRect, btRounded, btRect, btEllipse);
  TRoundedCorner = (rcTopLeft, rcTopRight, rcBottomRight, rcBottomLeft);
  TRoundedCorners = set of TRoundedCorner;
const
  ALL_ROUNDED_CORNERS = [rcTopLeft, rcTopRight, rcBottomLeft, rcBottomRight];

Type
  //Type of Draw for Border
  TBorderDrawStyle = (brdClear, brdSolid); //similar to Pen.psClear and Pen.psSolid
  TButtonDrawStyle = (btnClear, btnSolid); //similar to Brush.bsClear and Brush.bsSolid

  //List of available elements
  TButtonFamilies = array of TStyledButtonFamily;
  TButtonClasses = Array of TStyledButtonClass;
  TButtonAppearances = Array of TStyledButtonAppearance;

  TNotificationBadgeAttributes = class(TComponent)
  private
    FNotificationCount: Integer;
    FCustomText: string;
    FMaxNotifications: Word;
    FPosition: TNotificationBadgePosition;
    FSize: TNotificationBadgeSize;
    FColor: TColor;
    FFontColor: TColor;
    FFontStyle: TFontStyles;
    FOwnerControl: TControl;
    FOnContentChange: TNotifyEvent;
    procedure InvalidateControl;
    procedure SetMaxNotifications(const AValue: Word);
    procedure SetPosition(const AValue: TNotificationBadgePosition);
    procedure SetNotificationCount(const AValue: Integer);
    procedure SetColor(const AValue: TColor);
    procedure SetFontColor(const AValue: TColor);
    procedure SetFontStyle(const AValue: TFontStyles);
    function GetBadgeContent: string;
    procedure SetCustomText(const AValue: string);
    procedure SetSize(const Value: TNotificationBadgeSize);
    function GetIsVisible: Boolean;
    function IsFontStyleStored: Boolean;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    function HasCustomAttributes: Boolean;
    property BadgeContent: string read GetBadgeContent;
    property IsVisible: Boolean read GetIsVisible;
    property OwnerControl: TControl read FOwnerControl;
  published
    property Color: TColor read FColor write SetColor default DEFAULT_BADGE_COLOR;
    property CustomText: string read FCustomText write SetCustomText;
    property FontColor: TColor read FFontColor write SetFontColor default DEFAULT_BADGE_FONT_COLOR;
    property FontStyle: TFontStyles read FFontStyle write SetFontStyle stored IsFontStyleStored;
    property NotificationCount: Integer read FNotificationCount write SetNotificationCount default 0;
    property MaxNotifications: Word read FMaxNotifications write SetMaxNotifications default DEFAULT_MAX_BADGE_VALUE;
    property Position: TNotificationBadgePosition read FPosition write SetPosition default nbpTopRight;
    property Size: TNotificationBadgeSize read FSize write SetSize default nbsNormal;

    property OnContentChange: TNotifyEvent read FOnContentChange write FOnContentChange;
  end;

  TStyledButtonAttributes = class(TComponent)
  private
    //Custom values
    FCustomDrawType: TStyledButtonDrawType;
    FCustomBorderWidth: Integer;
    FCustomBorderDrawStyle: TBorderDrawStyle;
    FCustomButtonDrawStyle: TButtonDrawStyle;
    FCustomBorderColor: TColor;
    FCustomFontColor: TColor;
    FCustomFontStyle: TFontStyles;
    FCustomButtonColor: TColor;
    FCustomRadius: Integer;
    FCustomRoundedCorners: TRoundedCorners;

    //Default Values retrieved by Family/Class/Appearance
    FDrawType: TStyledButtonDrawType;
    FBorderWidth: Integer;
    FBorderDrawStyle: TBorderDrawStyle;
    FButtonDrawStyle: TButtonDrawStyle;
    FBorderColor: TColor;
    FFontColor: TColor;
    FFontStyle: TFontStyles;
    FFontName: TFontName;
    FButtonColor: TColor;
    FRadius: Integer;
    FRoundedCorners: TRoundedCorners;

    FOwnerControl: TControl;
    FHasCustomDrawType: Boolean;
    FHasCustomBorderWidth: Boolean;
    FHasCustomBorderDrawStyle: Boolean;
    FHasCustomButtonDrawStyle: Boolean;
    FHasCustomBorderColor: Boolean;
    FHasCustomFontColor: Boolean;
    FHasCustomFontStyle: Boolean;
    FHasCustomButtonColor: Boolean;
    FHasCustomRadius: Boolean;
    FHasCustomRoundedCorners: Boolean;

    procedure InvalidateControl;
    procedure SetBorderColor(const AValue: TColor);
    procedure SetBorderDrawStyle(const AValue: TBorderDrawStyle);
    procedure SetButtonDrawStyle(const AValue: TButtonDrawStyle);
    procedure SetDrawType(const AValue: TStyledButtonDrawType);
    procedure SetBorderWidth(const AValue: Integer);
    procedure SetButtonColor(const AValue: TColor);
    procedure SetFontColor(const AValue: TColor);
    procedure SetFontStyle(const AValue: TFontStyles);
    procedure SetRadius(const AValue: Integer);
    procedure SetRoundedCorners(const AValue: TRoundedCorners);
    function GetBorderColor: TColor;
    function GetBorderDrawStyle: TBorderDrawStyle;
    function GetBorderWidth: Integer;
    function GetButtonColor: TColor;
    function GetButtonDrawStyle: TButtonDrawStyle;
    function GetDrawType: TStyledButtonDrawType;
    function GetFontColor: TColor;
    function GetFontStyle: TFontStyles;
    function GetRadius: Integer;
    function GetRoundedCorners: TRoundedCorners;
    procedure SetCustomAttributes(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    function HasCustomAttributes: Boolean;
    procedure Assign(ASource: TPersistent); override;
    function PenStyle: TPenStyle;
    function BrushStyle: TBrushStyle;
    function AssignStyledAttributes(const ASource: TStyledButtonAttributes): Boolean;
    procedure ResetCustomAttributes;
    property HasCustomDrawType: Boolean read FHasCustomDrawType;
    property HasCustomBorderWidth: Boolean read FHasCustomBorderWidth;
    property HasCustomBorderDrawStyle: Boolean read FHasCustomBorderDrawStyle;
    property HasCustomButtonDrawStyle: Boolean read FHasCustomButtonDrawStyle;
    property HasCustomBorderColor: Boolean read FHasCustomBorderColor;
    property HasCustomFontColor: Boolean read FHasCustomFontColor;
    property HasCustomFontStyle: Boolean read FHasCustomFontStyle;
    property HasCustomButtonColor: Boolean read FHasCustomButtonColor;
    property HasCustomRadius: Boolean read FHasCustomRadius;
    property HasCustomRoundedCorners: Boolean read FHasCustomRoundedCorners;
  published
    property DrawType: TStyledButtonDrawType read GetDrawType write SetDrawType stored FHasCustomDrawType;
    property BorderWidth: Integer read GetBorderWidth write SetBorderWidth stored FHasCustomBorderWidth;
    property BorderDrawStyle: TBorderDrawStyle read GetBorderDrawStyle write SetBorderDrawStyle stored FHasCustomBorderDrawStyle;
    property ButtonDrawStyle: TButtonDrawStyle read GetButtonDrawStyle write SetButtonDrawStyle stored FHasCustomButtonDrawStyle;
    property BorderColor: TColor read GetBorderColor write SetBorderColor stored FHasCustomBorderColor;
    property FontColor: TColor read GetFontColor write SetFontColor stored FHasCustomFontColor;
    property FontStyle: TFontStyles read GetFontStyle write SetFontStyle stored FHasCustomFontStyle;
    property ButtonColor: TColor read GetButtonColor write SetButtonColor stored FHasCustomButtonColor;
    property Radius: Integer read GetRadius write SetRadius stored FHasCustomRadius;
    property RoundedCorners: TRoundedCorners read GetRoundedCorners write SetRoundedCorners stored FHasCustomRoundedCorners;
    property UseCustomAttributes: Boolean read HasCustomAttributes write SetCustomAttributes stored False;
  end;

  //  Abstraction of Graphic Button Attributes
  IStyledButtonAttributes = interface
    ['{C7BC98EE-D513-46B9-881A-2FDE8DE07786}']
    procedure UpdateAttributes(
      const AFamily:  TStyledButtonFamily;
      const AStyle: TStyledButtonClass;
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

  TButtonFamily = class(TObject)
  private
    FStyleFamily: TStyledButtonFamily;
    FCustomAttributes: IStyledButtonAttributes;
  public
    property StyledAttributes: IStyledButtonAttributes read FCustomAttributes;
  end;

//utilities
procedure CheckValue(const AName: string; const AValue, AMin, AMax: Integer);
function DarkenColor(Color:TColor; Percent:integer):TColor;
function LightenColor(Color:TColor; Percent:integer):TColor;
function HtmlToColor(Color: string): TColor;
function ColortoGrayscale(AColor : TColor): TColor;
function ColorIsLight(Color: TColor): Boolean;
function SameStyledButtonStyle(Style1, Style2: TStyledButtonAttributes): Boolean;
function SameNotificationBadgeAttributes(Attr1, Attr2: TNotificationBadgeAttributes): Boolean;
procedure CloneButtonStyle(const ASource: TStyledButtonAttributes;
  var ADest: TStyledButtonAttributes);
function GetActiveStyleName(const AControl: TControl): string;
function GetWindowsVersion: TWindowsVersion;
function ClearHRefs(const Msg: string;  OnlyLinks: boolean = True;
  OnlyFileNotExists: boolean = False): string;

//Calculate Image and Text Rect for Drawing using ImageAlignment and ImageMargins
//for StyledButton and StyledGraphicButton
procedure CalcImageAndTextRect(const ASurfaceRect: TRect;
  const ACaption: string;
  out ATextRect: TRect; out AImageRect: TRect;
  const AImageWidth, AImageHeight: Integer;
  const AImageAlignment: TImageAlignment;
  const AImageMargins: TImageMargins;
  const AScale: Single); overload;

//Calculate Image and Text Rect for Drawing using ButtonLayout, Margin and Spacing
//For StyledSpeedButton and StyledBitBtn
procedure CalcImageAndTextRect(const ACanvas: TCanvas;
  const ACaption: string; const AClient: TRect;
  const AOffset: TPoint;
  var AGlyphPos: TPoint; var ATextBounds: TRect;
  const AImageWidth, AImageHeight: Integer;
  const ALayout: TButtonLayout;
  const AMargin, ASpacing: Integer;
  const ABiDiFlags: Cardinal); overload;

//draw of Glyph
procedure DrawBitBtnGlyph(ACanvas: TCanvas; ARect: TRect;
  Kind: Vcl.Buttons.TBitBtnKind;
  AState: TButtonState; AEnabled: Boolean;
  AOriginal: TBitmap; ANumGlyphs: Integer; const ATransparentColor: TColor);

//drawing a Text in a Canvas Using Alignment and Spacing
procedure DrawButtonText(const ACanvas: TCanvas;
  const AText: string; const AAlignment: TAlignment;
  const ASpacing: Integer;
  var ARect: TRect; AFlags: Cardinal);

//drawing a Notification Badge on a Canvas
procedure DrawButtonNotificationBadge(const ACanvas: TCanvas;
  const ASurfaceRect: TRect; const AScaleFactor: Single;
  const AValue: string;
  const ASizeType: TNotificationBadgeSize;
  const APosition: TNotificationBadgePosition;
  const AColor, AFontColor: TColor; const AFontStyle: TFontStyles);

//drawing "old-style" with masked bitmap
procedure DrawBitmapTransparent(ACanvas: TCanvas; ARect: TRect;
  const AWidth, AHeight: Integer; AOriginal: TBitmap;
  AState: TButtonState; ANumGlyphs: Integer; const ATransparentColor: TColor);

//drawing Command-Link Arrow (white or Black)
procedure DrawIconFromCommandLinkRes(ACanvas: TCanvas; ARect: TRect;
  AVCLStyleName: string; AState: TButtonState; AEnabled: Boolean);

//Draw rectangle and border into Canvas
procedure DrawRect(ACanvas: TCanvas; var ARect: TRect);

//draw Button into Canvas
procedure CanvasDrawShape(const ACanvas: TCanvas; ARect: TRect;
  const ADrawType: TStyledButtonDrawType; const ACornerRadius: Single;
  const ARoundedCorners: TRoundedCorners;
  const APreserveBorderSpace: Boolean = True);

//draw Text into Canvas
procedure CanvasDrawText(const ACanvas: TCanvas; ARect: TRect;
  const AText: string; ABiDiModeFlags: LongInt);

//draw bar and triangle for SplitButton into Canvas
procedure CanvasDrawBarAndTriangle(const ACanvas: TCanvas; const ARect: TRect;
  const AScaleFactor: Single; ABarColor, ATriangleColor: TColor);

//draw Vertical bar into Canvas
procedure CanvasDrawBar(const ACanvas: TCanvas; const ARect: TRect;
  const AScaleFactor: Single; ABarColor: TColor);

//draw a triangle into Canvas
procedure CanvasDrawTriangle(const ACanvas: TCanvas; const ARect: TRect;
  const AScaleFactor: Single; ATriangleColor: TColor);

//ButtonFamily Factory
procedure RegisterButtonFamily(
  const AStyledButtonAttributes: IStyledButtonAttributes);
function GetButtonFamilies: TObjectList;
function GetButtonFamilyClass(const AFamilyName: TStyledButtonFamily): TButtonFamily;
function GetButtonFamilyName(const Index: Integer): TStyledButtonFamily;
function GetButtonClasses(const AFamily: TButtonFamily): TButtonClasses;
function GetButtonAppearances(const AFamily: TButtonFamily): TButtonAppearances;
function GetButtonFamilyClasses(const AFamily: TStyledButtonFamily): TButtonClasses;
function GetButtonFamilyAppearances(const AFamily: TStyledButtonFamily): TButtonAppearances;

function StyleFamilyCheckAttributes(
  const AFamily: TStyledButtonFamily;
  var AClass: TStyledButtonClass;
  var AAppearance: TStyledButtonAppearance;
  out AButtonFamily: TButtonFamily): Boolean;

procedure StyleFamilyUpdateAttributes(
  const AFamily: TStyledButtonFamily;
  var AClass: TStyledButtonClass;
  var AAppearance: TStyledButtonAppearance;
  var ANormalStyle, APressedStyle,
  ASelectedStyle, AHotStyle, ADisabledStyle: TStyledButtonAttributes);

procedure StyleFamilyUpdateAttributesByModalResult(
  const AModalResult: TModalResult;
  const AFamily: TStyledButtonFamily;
  var AClass: TStyledButtonClass;
  var AAppearance: TStyledButtonAppearance);

implementation

uses
  System.Win.Registry
  , System.Math
{$ifdef GDIPlusSupport}
  , Winapi.GDIPAPI
  , Winapi.GDIPOBJ
{$endif}
  , Vcl.StandardButtonStyles
  ;

var
  _WindowsVersion: TWindowsVersion;

procedure CheckValue(const AName: string; const AValue, AMin, AMax: Integer);
begin
  if (AValue < AMin) or (AValue > AMax) then
    raise EStyledAttributesException.CreateFmt(ERROR_VALUE_OUT_OF_RANGE,
      [AValue, AName, AMin, AMax]);
end;

function SameStyledButtonStyle(Style1, Style2: TStyledButtonAttributes): Boolean;
begin
  Result :=
    (Style1.FDrawType = Style2.FDrawType) and
    (Style1.FBorderWidth = Style2.FBorderWidth) and
    (Style1.FBorderDrawStyle = Style2.FBorderDrawStyle) and
    (Style1.FButtonDrawStyle = Style2.FButtonDrawStyle)  and
    (Style1.FBorderColor = Style2.FBorderColor) and
    (Style1.FFontColor = Style2.FFontColor) and
    (Style1.FFontStyle = Style2.FFontStyle) and
    (Style1.FFontName = Style2.FFontName) and
    (Style1.FButtonColor = Style2.FButtonColor) and
    (Style1.FRadius = Style2.FRadius) and
    (Style1.FRoundedCorners = Style2.FRoundedCorners);
end;

function SameNotificationBadgeAttributes(Attr1, Attr2: TNotificationBadgeAttributes): Boolean;
begin
  Result :=
    (Attr1.FNotificationCount = Attr2.FNotificationCount) and
    (Attr1.FMaxNotifications = Attr2.FMaxNotifications) and
    (Attr1.FPosition = Attr2.FPosition);
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
  ADest.FDrawType := ASource.FDrawType;
  ADest.FBorderWidth := ASource.FBorderWidth;
  ADest.FBorderDrawStyle := ASource.FBorderDrawStyle;
  ADest.FButtonDrawStyle := ASource.FButtonDrawStyle;
  ADest.FBorderColor := ASource.FBorderColor;
  ADest.FFontStyle := ASource.FFontStyle;
  ADest.FFontColor := ASource.FFontColor;
  ADest.FButtonColor := ASource.FButtonColor;
  ADest.FRadius := ASource.FRadius;
  ADest.FRoundedCorners := ASource.FRoundedCorners;

  ADest.FCustomDrawType := ASource.FCustomDrawType;
  ADest.FCustomBorderWidth := ASource.FCustomBorderWidth;
  ADest.FCustomBorderDrawStyle := ASource.FCustomBorderDrawStyle;
  ADest.FCustomButtonDrawStyle := ASource.FCustomButtonDrawStyle;
  ADest.FCustomBorderColor := ASource.FCustomBorderColor;
  ADest.FCustomFontStyle := ASource.FCustomFontStyle;
  ADest.FCustomFontColor := ASource.FCustomFontColor;
  ADest.FCustomButtonColor := ASource.FCustomButtonColor;
  ADest.FCustomRadius := ASource.FCustomRadius;
  ADest.FCustomRoundedCorners := ASource.FCustomRoundedCorners;

  ADest.FHasCustomDrawType := ASource.FHasCustomDrawType;
  ADest.FHasCustomBorderWidth := ASource.FHasCustomBorderWidth;
  ADest.FHasCustomBorderDrawStyle := ASource.FHasCustomBorderDrawStyle;
  ADest.FHasCustomButtonDrawStyle := ASource.FHasCustomButtonDrawStyle;
  ADest.FHasCustomBorderColor := ASource.FHasCustomBorderColor;
  ADest.FHasCustomFontStyle := ASource.FHasCustomFontStyle;
  ADest.FHasCustomFontColor := ASource.FHasCustomFontColor;
  ADest.FHasCustomButtonColor := ASource.FHasCustomButtonColor;
  ADest.FHasCustomRadius := ASource.FHasCustomRadius;
  ADest.FHasCustomRoundedCorners := ASource.FHasCustomRoundedCorners;
end;

function GetActiveStyleName(const AControl: TControl): string;
begin
  {$IFDEF D10_4+}
  Result := AControl.GetStyleName;
  if Result = '' then
  begin
    {$IFDEF D11+}
    if (csDesigning in AControl.ComponentState) then
      Result := TStyleManager.ActiveDesigningStyle.Name
    else
      Result := TStyleManager.ActiveStyle.Name;
    {$ELSE}
      Result := TStyleManager.ActiveStyle.Name;
    {$ENDIF}
  end;
  {$ELSE}
  Result := TStyleManager.ActiveStyle.Name;
  {$ENDIF}
  if (csDesigning in AControl.ComponentState) then
  begin
    if (Result = 'Windows Designer Dark') or
      (Result = 'Win10IDE_Dark') or
      (Result = 'Win10IDE_Light') or
      (Result = 'Mountain_Mist' ) then
      Result := 'Windows';
  end;
end;

function GetWindowsVersion: TWindowsVersion;
var
  Reg: TRegistry;
  VersionInfo: TOSVersionInfo;
  LBuildNumber: Integer;
begin
  if _WindowsVersion = wvUndefined then
  begin
    VersionInfo.dwOSVersionInfoSize := sizeOf(TOSVersionInfo);
    Reg := TRegistry.Create;
    Try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      case VersionInfo.dwPlatformID of
        VER_PLATFORM_WIN32_WINDOWS:
          Reg.OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion');
      else
        Reg.OpenKeyReadOnly('\Software\Microsoft\Windows NT\CurrentVersion');
      end;
      LBuildNumber := StrToIntDef(Reg.ReadString('CurrentBuild'), 0);
      if LBuildNumber >= 22000 then
        _WindowsVersion := wvWindows11
      else if LBuildNumber >= 10240 then
        _WindowsVersion := wvWindows10
      else if LBuildNumber >= 9600 then
        _WindowsVersion := wvWindows8_1
      else if LBuildNumber >= 9200 then
        _WindowsVersion := wvWindows8
      else if LBuildNumber >= 7600 then
        _WindowsVersion := wvWindows7
      else if LBuildNumber >= 6000 then
        _WindowsVersion := wvWindowsVista
      else if LBuildNumber >= 2600 then
        _WindowsVersion := wvWindowsXP;
      Reg.CloseKey;
    Finally
      Reg.Free;
    End;
  end;
  Result := _WindowsVersion;
end;

function ExtractHrefValues(const HRef: string;
  out LinkStr, DisplayLabel: string) : boolean;
var
  p1, p2, p3: integer;
begin
  p1 := pos('>', HRef);
  p2 := Length(HRef)-3;
  p3 := pos('">',HRef);
  if (p1 > 0) and (p3 > 0) and
    SameText(Copy(HRef,1,9),'<A HREF="') and
    SameText(Copy(HRef,p2,4),'</A>') then
  begin
    DisplayLabel := Copy(HRef,p1+1,p2-p1-1);
    LinkStr := Copy(HRef,10,p1-11);
    Result := True;
  end
  else
  begin
    LinkStr := HRef;
    DisplayLabel := '';
    Result := False;
  end;
end;

function HRefToString(const HRef: string): string;
var
  DisplayLabel: string;
  LinkStr: string;
begin
  //input: '<A HREF="c:\windows\system32\Notepad.exe'>Editor</A>'
  //output: 'Editor (c:\windows\system32\Notepad.exe)';

  if ExtractHrefValues(HRef, DisplayLabel, LinkStr) then
  begin
    if not SameText(DisplayLabel, LinkStr) then
      Result := Format('%s (%s)',[LinkStr,DisplayLabel])
    else
      Result := LinkStr;
  end
  else
    Result := HRef;
end;

function HRefToLinkStr(const HRef: string): string;
var
  DisplayLabel: string;
  LinkStr: string;
begin
  //input: '<A HREF="c:\windows\system32\Notepad.exe'>Editor</A>'
  //output: 'Editor';
  if ExtractHrefValues(HRef, DisplayLabel, LinkStr) then
  begin
    Result := LinkStr;
  end
  else
    Result := HRef;
end;

function ClearHRefs(const Msg: string; OnlyLinks: boolean = True;
  OnlyFileNotExists: boolean = False): string;
var
  p1, p2: integer;
  SubMsg, HRef, LinkStr, DisplayLabel: string;
begin
  Result := '';
  SubMsg := Msg;
  while True do
  begin
    p1 := pos('<A HREF="', UpperCase(SubMsg));
    p2 := pos('</A>', UpperCase(SubMsg));
    if (p1 > 0) and (p2 > 0) then
    begin
      HRef := Copy(SubMsg, p1, Succ(p2+3-p1));
      ExtractHrefValues(HRef, LinkStr, DisplayLabel);
      if not OnlyFileNotExists or not FileExists(LinkStr) then
      begin
        if OnlyLinks then
          Result := Result + Copy(SubMsg,1,p1-1)+HRefToLinkStr(HRef)
        else
          Result := Result + Copy(SubMsg,1,p1-1)+HRefToString(HRef);
      end
      else
        Result := Result + Copy(SubMsg,1,p1-1)+HRef;
      SubMsg := Copy(SubMsg,p2+4,maxint);
    end
    else
    begin
      Result := Result + SubMsg;
      break;
    end;
  end;
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

function StyleFamilyCheckAttributes(
  const AFamily: TStyledButtonFamily;
  var AClass: TStyledButtonClass;
  var AAppearance: TStyledButtonAppearance;
  out AButtonFamily: TButtonFamily): Boolean;
var
  I: Integer;
  LClasses: TButtonClasses;
  LAppearances: TButtonAppearances;
  LDefaultClass: TStyledButtonClass;
  LDefaultAppearance: TStyledButtonAppearance;
  LClassFound, LAppearanceFound: Boolean;
begin
  Result := True;
  if GetButtonFamily(AFamily, AButtonFamily) then
  begin
    AButtonFamily.FCustomAttributes.GetStyleByModalResult(mrNone,
       LDefaultClass, LDefaultAppearance);
    //Check AClass
    LClassFound := False;
    LClasses := AButtonFamily.FCustomAttributes.GetButtonClasses;
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
    begin
      AClass := LDefaultClass;
      Result := False;
    end;

    //Check AAppearance
    LAppearanceFound := False;
    LAppearances := AButtonFamily.FCustomAttributes.GetButtonAppearances;
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
    begin
      AAppearance := LDefaultAppearance;
      Result := False;
    end;
  end;
end;

procedure StyleFamilyUpdateAttributes(
  const AFamily: TStyledButtonFamily;
  var AClass: TStyledButtonClass;
  var AAppearance: TStyledButtonAppearance;
  var ANormalStyle, APressedStyle,
  ASelectedStyle, AHotStyle, ADisabledStyle: TStyledButtonAttributes);
var
  LButtonFamily: TButtonFamily;
  LNormalStyle, LPressedStyle, LSelectedStyle, LHotStyle, LDisabledStyle: TStyledButtonAttributes;
begin
  if GetButtonFamily(AFamily, LButtonFamily) then
  begin
    LNormalStyle := TStyledButtonAttributes.Create(nil);
    LPressedStyle := TStyledButtonAttributes.Create(nil);
    LSelectedStyle := TStyledButtonAttributes.Create(nil);
    LHotStyle := TStyledButtonAttributes.Create(nil);
    LDisabledStyle := TStyledButtonAttributes.Create(nil);
    try
      LButtonFamily.FCustomAttributes.UpdateAttributes(
        AFamily, AClass, AAppearance,
        LNormalStyle, LPressedStyle, LSelectedStyle,
        LHotStyle, LDisabledStyle);

      ANormalStyle.AssignStyledAttributes(LNormalStyle);
      APressedStyle.AssignStyledAttributes(LPressedStyle);
      ASelectedStyle.AssignStyledAttributes(LSelectedStyle);
      AHotStyle.AssignStyledAttributes(LHotStyle);
      ADisabledStyle.AssignStyledAttributes(LDisabledStyle);
    finally
      LNormalStyle.Free;
      LPressedStyle.Free;
      LSelectedStyle.Free;
      LHotStyle.Free;
      LDisabledStyle.Free;
    end;
  end;
end;

procedure StyleFamilyUpdateAttributesByModalResult(
  const AModalResult: TModalResult;
  const AFamily: TStyledButtonFamily;
  var AClass: TStyledButtonClass;
  var AAppearance: TStyledButtonAppearance);
var
  LButtonFamily: TButtonFamily;
begin
  if GetButtonFamily(AFamily, LButtonFamily) then
  begin
    LButtonFamily.FCustomAttributes.GetStyleByModalResult(
      AModalResult,
      AClass, AAppearance);
  end;
end;

procedure RegisterButtonFamily(
  const AStyledButtonAttributes: IStyledButtonAttributes);
var
  LFamily: TButtonFamily;
begin
  LFamily := TButtonFamily.Create;
  LFamily.FStyleFamily := AStyledButtonAttributes.ButtonFamilyName;
  LFamily.FCustomAttributes := AStyledButtonAttributes;
  if not Assigned(FFamilies) then
    FFamilies := TObjectList.Create(True);
  FFamilies.Add(LFamily);
end;

function GetButtonFamilies: TObjectList;
begin
  Result := FFamilies;
end;

function GetButtonFamilyClass(const AFamilyName: TStyledButtonFamily): TButtonFamily;
begin
  if not GetButtonFamily(AFamilyName, Result) then
    raise EStyledAttributesException.CreateFmt(ERROR_FAMILY_NOT_FOUND,[AFamilyName]);
end;

function GetButtonClasses(const AFamily: TButtonFamily): TButtonClasses;
begin
  Result := AFamily.FCustomAttributes.GetButtonClasses;
end;

function GetButtonAppearances(const AFamily: TButtonFamily): TButtonAppearances;
begin
  Result := AFamily.FCustomAttributes.GetButtonAppearances;
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
    Result := LButtonFamily.FCustomAttributes.GetButtonClasses
  else
    raise EStyledAttributesException.CreateFmt(ERROR_FAMILY_NOT_FOUND,[AFamily]);
end;

function GetButtonFamilyAppearances(const AFamily: TStyledButtonFamily): TButtonAppearances;
var
  LButtonFamily: TButtonFamily;
begin
  if GetButtonFamily(AFamily, LButtonFamily) then
    Result := LButtonFamily.FCustomAttributes.GetButtonAppearances
  else
    raise EStyledAttributesException.CreateFmt(ERROR_FAMILY_NOT_FOUND,[AFamily]);
end;

{ TNotificationBadgeAttributes }

procedure TNotificationBadgeAttributes.Assign(ASource: TPersistent);
var
  LSource: TNotificationBadgeAttributes;
begin
  if ASource is TNotificationBadgeAttributes then
  begin
    LSource := TNotificationBadgeAttributes(ASource);
    NotificationCount := LSource.FNotificationCount;
    MaxNotifications := LSource.FMaxNotifications;
    Position := LSource.FPosition;
    Color := LSource.FColor;
    FontColor := LSource.FFontColor;
    Size := LSource.FSize;
  end
  else
    inherited Assign(ASource);
end;

constructor TNotificationBadgeAttributes.Create(AOwner: TComponent);
begin
  inherited;
  FNotificationCount := 0;
  FMaxNotifications := DEFAULT_MAX_BADGE_VALUE;
  FPosition := nbpTopRight;
  FColor := DEFAULT_BADGE_COLOR;
  FFontColor := DEFAULT_BADGE_FONT_COLOR;
  FFontStyle := [System.UITypes.TFontStyle.fsBold];
  FSize := nbsNormal;
  if AOwner is TControl then
  begin
    FOwnerControl := TControl(AOwner);
    SetSubComponent(True);
  end;
end;

function TNotificationBadgeAttributes.GetBadgeContent: string;
begin
  if (FCustomText <> '')  then
    Result := FCustomText
  else
  begin
    if FNotificationCount > MaxNotifications then
      Result := IntToStr(MaxNotifications)+'+'
    else if FNotificationCount > 0 then
      Result := IntToStr(FNotificationCount)
    else
      Result := '';
  end;
end;

function TNotificationBadgeAttributes.GetIsVisible: Boolean;
begin
  Result := (FCustomText <> '') or (FNotificationCount > 0);
end;

function TNotificationBadgeAttributes.HasCustomAttributes: Boolean;
begin
  Result := (FNotificationCount <> 0) or
    (FMaxNotifications <> DEFAULT_MAX_BADGE_VALUE) or
    (FPosition <> nbpTopRight) or
    (FColor <> DEFAULT_BADGE_COLOR) or
    (FFontColor <> DEFAULT_BADGE_FONT_COLOR) or
    (FSize <> nbsNormal) or
    (FCustomText <> '');
end;

procedure TNotificationBadgeAttributes.InvalidateControl;
begin
  if Assigned(FOwnerControl) then
    FOwnerControl.Invalidate;
end;

function TNotificationBadgeAttributes.IsFontStyleStored: Boolean;
var
  LFontStyle : TFontStyles;
begin
  LFontStyle := [System.UITypes.TFontStyle.fsBold];
  Result := FFontStyle <> LFontStyle;
end;

procedure TNotificationBadgeAttributes.SetMaxNotifications(const AValue: Word);
begin
  if FMaxNotifications <> AValue then
  begin
    FMaxNotifications := AValue;
    if IsVisible then
      InvalidateControl;
  end;
end;

procedure TNotificationBadgeAttributes.SetPosition(const AValue: TNotificationBadgePosition);
begin
  if FPosition <> AValue then
  begin
    FPosition := AValue;
    if IsVisible then
      InvalidateControl;
  end;
end;

procedure TNotificationBadgeAttributes.SetSize(
  const Value: TNotificationBadgeSize);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    InvalidateControl;
  end;
end;

procedure TNotificationBadgeAttributes.SetCustomText(const AValue: string);
begin
  if FCustomText <> AValue then
  begin
    FCustomText := AValue;
    if Assigned(FOnContentChange) then
      FOnContentChange(Self);
    InvalidateControl;
  end;
end;

procedure TNotificationBadgeAttributes.SetColor(const AValue: TColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    if IsVisible then
      InvalidateControl;
  end;
end;

procedure TNotificationBadgeAttributes.SetNotificationCount(const AValue: Integer);
begin
  if AValue < 0 then
    raise EStyledAttributesException.Create(ERROR_NEGATIVE_VALUE);
  if FNotificationCount <> AValue then
  begin
    FNotificationCount := AValue;
    if Assigned(FOnContentChange) then
      FOnContentChange(Self);
    InvalidateControl;
  end;
end;

procedure TNotificationBadgeAttributes.SetFontColor(const AValue: TColor);
begin
  if FFontColor <> AValue then
  begin
    FFontColor := AValue;
    if IsVisible then
      InvalidateControl;
  end;
end;

procedure TNotificationBadgeAttributes.SetFontStyle(const AValue: TFontStyles);
begin
  if FFontStyle <> AValue then
  begin
    FFontStyle := AValue;
    if IsVisible then
      InvalidateControl;
  end;
end;

{ TStyledButtonAttributes }

procedure TStyledButtonAttributes.SetCustomAttributes(const Value: Boolean);
begin
  if not Value then
  begin
    ResetCustomAttributes;
    InvalidateControl;
  end;
end;

function TStyledButtonAttributes.HasCustomAttributes: Boolean;
begin
  Result := FHasCustomDrawType or
    FHasCustomBorderWidth or
    FHasCustomBorderDrawStyle or
    FHasCustomButtonDrawStyle or
    FHasCustomBorderColor or
    FHasCustomFontColor or
    FHasCustomFontStyle or
    FHasCustomButtonColor or
    FHasCustomRadius or
    FHasCustomRoundedCorners;
end;

function TStyledButtonAttributes.PenStyle: TPenStyle;
begin
  case BorderDrawStyle of
    brdClear: Result := psClear;
  else
    Result := psSolid;
  end;
end;

procedure TStyledButtonAttributes.ResetCustomAttributes;
begin
  FHasCustomDrawType := False;
  FHasCustomBorderWidth := False;
  FHasCustomBorderDrawStyle := False;
  FHasCustomButtonDrawStyle := False;
  FHasCustomBorderColor := False;
  FHasCustomFontColor := False;
  FHasCustomFontStyle := False;
  FHasCustomButtonColor := False;
  FHasCustomRadius := False;
  FHasCustomRoundedCorners := False;
end;

procedure TStyledButtonAttributes.Assign(ASource: TPersistent);
var
  LSource: TStyledButtonAttributes;
begin
  if ASource is TStyledButtonAttributes then
  begin
    LSource := TStyledButtonAttributes(ASource);
    CloneButtonStyle(LSource, Self);
  end
  else
    inherited Assign(ASource);
end;

function TStyledButtonAttributes.AssignStyledAttributes(
  const ASource: TStyledButtonAttributes): Boolean;
begin
  //Assign internal "custom" variable
(*
  FCustomDrawType := ASource.FCustomDrawType;
  FCustomBorderWidth := ASource.FCustomBorderWidth;
  FCustomBorderDrawStyle := ASource.FCustomBorderDrawStyle;
  FCustomButtonDrawStyle := ASource.FCustomButtonDrawStyle;
  FCustomBorderColor := ASource.FCustomBorderColor;
  FCustomFontColor := ASource.FCustomFontColor;
  FCustomFontStyle := ASource.FCustomFontStyle;
  FCustomButtonColor := ASource.FCustomButtonColor;
  FCustomRadius := ASource.FCustomRadius;
  FCustomRoundedCorners := ASource.FCustomRoundedCorners;
*)
  //Assign internal variable
  FDrawType := ASource.FDrawType;
  FBorderWidth := ASource.FBorderWidth;
  FBorderDrawStyle := ASource.FBorderDrawStyle;
  FButtonDrawStyle := ASource.FButtonDrawStyle;
  FBorderColor := ASource.FBorderColor;
  FFontColor := ASource.FFontColor;
  FFontStyle := ASource.FFontStyle;
  FButtonColor := ASource.FButtonColor;
  FRadius := ASource.FRadius;
  FRoundedCorners := ASource.FRoundedCorners;
  Result := True;
end;

function TStyledButtonAttributes.BrushStyle: TBrushStyle;
begin
  case FButtonDrawStyle of
    btnClear: Result := bsClear;
  else
    Result := bsSolid;
  end;
end;

constructor TStyledButtonAttributes.Create(AOwner: TComponent);
begin
  inherited;
  FRadius := DEFAULT_RADIUS;
  FRoundedCorners := ALL_ROUNDED_CORNERS;
  FBorderDrawStyle := brdSolid;
  FButtonDrawStyle := btnSolid;

  if AOwner is TControl then
  begin
    FOwnerControl := TControl(AOwner);
    SetSubComponent(True);
  end;
end;

function TStyledButtonAttributes.GetBorderColor: TColor;
begin
  if not HasCustomBorderColor then
    Result := FBorderColor
  else
    Result := FCustomBorderColor;
end;

function TStyledButtonAttributes.GetBorderDrawStyle: TBorderDrawStyle;
begin
  if not HasCustomBorderDrawStyle then
    Result := FBorderDrawStyle
  else
    Result := FCustomBorderDrawStyle;
end;

function TStyledButtonAttributes.GetBorderWidth: Integer;
begin
  if not HasCustomBorderWidth then
    Result := FBorderWidth
  else
    Result := FCustomBorderWidth;
end;

function TStyledButtonAttributes.GetButtonColor: TColor;
begin
  if not HasCustomButtonColor then
    Result := FButtonColor
  else
    Result := FCustomButtonColor;
end;

function TStyledButtonAttributes.GetButtonDrawStyle: TButtonDrawStyle;
begin
  if not HasCustomButtonDrawStyle then
    Result := FButtonDrawStyle
  else
    Result := FCustomButtonDrawStyle;
end;

function TStyledButtonAttributes.GetDrawType: TStyledButtonDrawType;
begin
  if not HasCustomDrawType then
    Result := FDrawType
  else
    Result := FCustomDrawType;
end;

function TStyledButtonAttributes.GetFontColor: TColor;
begin
  if not HasCustomFontColor then
    Result := FFontColor
  else
    Result := FCustomFontColor;
end;

function TStyledButtonAttributes.GetFontStyle: TFontStyles;
begin
  if not HasCustomFontStyle then
    Result := FFontStyle
  else
    Result := FCustomFontStyle;
end;

function TStyledButtonAttributes.GetRadius: Integer;
begin
  if not HasCustomRadius then
    Result := FRadius
  else
    Result := FCustomRadius;
end;

function TStyledButtonAttributes.GetRoundedCorners: TRoundedCorners;
begin
  if not HasCustomRoundedCorners then
    Result := FRoundedCorners
  else
    Result := FCustomRoundedCorners;
end;

procedure TStyledButtonAttributes.InvalidateControl;
begin
  if Assigned(FOwnerControl) then
    FOwnerControl.Invalidate;
end;

procedure TStyledButtonAttributes.SetBorderColor(const AValue: TColor);
begin
  if Assigned(FOwnerControl) then
  begin
    //Setting a Custom property value
    if FCustomBorderColor <> AValue then
    begin
      FCustomBorderColor := AValue;
      FHasCustomBorderColor := FCustomBorderColor <> FBorderColor;
      InvalidateControl;
    end
  end
  else
  begin
    //Setting a property using StyleFamily, StyleClass and StyleAppearance
    if FBorderColor <> AValue then
    begin
      FBorderColor := AValue;
      InvalidateControl;
    end;
  end;
end;

procedure TStyledButtonAttributes.SetBorderDrawStyle(const AValue: TBorderDrawStyle);
begin
  if Assigned(FOwnerControl) then
  begin
    //Setting a Custom property value
    if FCustomBorderDrawStyle <> AValue then
    begin
      FCustomBorderDrawStyle := AValue;
      FHasCustomBorderDrawStyle := FCustomBorderDrawStyle <> FBorderDrawStyle;
      InvalidateControl;
    end
  end
  else
  begin
    //Setting a property using StyleFamily, StyleClass and StyleAppearance
    if FBorderDrawStyle <> AValue then
    begin
      FBorderDrawStyle := AValue;
      InvalidateControl;
    end;
  end;
end;

procedure TStyledButtonAttributes.SetButtonDrawStyle(const AValue: TButtonDrawStyle);
begin
  if Assigned(FOwnerControl) then
  begin
    //Setting a Custom property value
    if FCustomButtonDrawStyle <> AValue then
    begin
      FCustomButtonDrawStyle := AValue;
      FHasCustomButtonDrawStyle := FCustomButtonDrawStyle <> FButtonDrawStyle;
      InvalidateControl;
    end
  end
  else
  begin
    //Setting a property using StyleFamily, StyleClass and StyleAppearance
    if FButtonDrawStyle <> AValue then
    begin
      FButtonDrawStyle := AValue;
      InvalidateControl;
    end;
  end;
end;

procedure TStyledButtonAttributes.SetDrawType(const AValue: TStyledButtonDrawType);
begin
  if Assigned(FOwnerControl) then
  begin
    //Setting a Custom property value
    if FCustomDrawType <> AValue then
    begin
      FCustomDrawType := AValue;
      FHasCustomDrawType := FCustomDrawType <> FDrawType;
      InvalidateControl;
    end
  end
  else
  begin
    //Setting a property using StyleFamily, StyleClass and StyleAppearance
    if FDrawType <> AValue then
    begin
      FDrawType := AValue;
      InvalidateControl;
    end;
  end;
end;

procedure TStyledButtonAttributes.SetBorderWidth(const AValue: Integer);
begin
  if Assigned(FOwnerControl) then
  begin
    //Setting a Custom property value
    if FCustomBorderWidth <> AValue then
    begin
      FCustomBorderWidth := AValue;
      FHasCustomBorderWidth := FCustomBorderWidth <> FBorderWidth;
      InvalidateControl;
    end
  end
  else
  begin
    //Setting a property using StyleFamily, StyleClass and StyleAppearance
    if FBorderWidth <> AValue then
    begin
      FBorderWidth := AValue;
      InvalidateControl;
    end;
  end;
end;

procedure TStyledButtonAttributes.SetButtonColor(const AValue: TColor);
begin
  if Assigned(FOwnerControl) then
  begin
    //Setting a Custom property value
    if FCustomButtonColor <> AValue then
    begin
      FCustomButtonColor := AValue;
      FHasCustomButtonColor := FCustomButtonColor <> FButtonColor;
      InvalidateControl;
    end
  end
  else
  begin
    //Setting a property using StyleFamily, StyleClass and StyleAppearance
    if FButtonColor <> AValue then
    begin
      FButtonColor := AValue;
      InvalidateControl;
    end;
  end;
end;

procedure TStyledButtonAttributes.SetFontColor(const AValue: TColor);
begin
  if Assigned(FOwnerControl) then
  begin
    //Setting a Custom property value
    if FCustomFontColor <> AValue then
    begin
      FCustomFontColor := AValue;
      FHasCustomFontColor := FCustomFontColor <> FFontColor;
      InvalidateControl;
    end
  end
  else
  begin
    //Setting a property using StyleFamily, StyleClass and StyleAppearance
    if FFontColor <> AValue then
    begin
      FFontColor := AValue;
      InvalidateControl;
    end;
  end;
end;

procedure TStyledButtonAttributes.SetFontStyle(const AValue: TFontStyles);
begin
  if Assigned(FOwnerControl) then
  begin
    //Setting a Custom property value
    if FCustomFontStyle <> AValue then
    begin
      FCustomFontStyle := AValue;
      FHasCustomFontStyle := FCustomFontStyle <> FFontStyle;
      InvalidateControl;
    end
  end
  else
  begin
    //Setting a property using StyleFamily, StyleClass and StyleAppearance
    if FFontStyle <> AValue then
    begin
      FFontStyle := AValue;
      InvalidateControl;
    end;
  end;
end;

procedure TStyledButtonAttributes.SetRadius(const AValue: Integer);
begin
  if Assigned(FOwnerControl) then
  begin
    //Setting a Custom property value
    if FCustomRadius <> AValue then
    begin
      FCustomRadius := AValue;
      FHasCustomRadius := FCustomRadius <> FRadius;
      InvalidateControl;
    end
  end
  else
  begin
    //Setting a property using StyleFamily, StyleClass and StyleAppearance
    if FRadius <> AValue then
    begin
      FRadius := AValue;
      InvalidateControl;
    end;
  end;
end;

procedure TStyledButtonAttributes.SetRoundedCorners(const AValue: TRoundedCorners);
begin
  if Assigned(FOwnerControl) then
  begin
    //Setting a Custom property value
    if FCustomRoundedCorners <> AValue then
    begin
      FCustomRoundedCorners := AValue;
      FHasCustomRoundedCorners := FCustomRoundedCorners <> FRoundedCorners;
      InvalidateControl;
    end
  end
  else
  begin
    //Setting a property using StyleFamily, StyleClass and StyleAppearance
    if FRoundedCorners <> AValue then
    begin
      FRoundedCorners := AValue;
      InvalidateControl;
    end;
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

{$ifdef GDIPlusSupport}
(*
function GetRoundedPath(ARectangle: TGPRectF;
  ARadius: Single): TGPGraphicsPath;
var
  LPath : TGPGraphicsPath;
  l, t, w, h, d : Single;
begin
  LPath := TGPGraphicsPath.Create;
  l := ARectangle.X;
  t := ARectangle.y;
  w := ARectangle.Width;
  h := ARectangle.Height;
  d := ARadius / 2;

  // the lines beween the arcs are automatically added by the path
  LPath.AddArc(l, t, d, d, 180, 90); // topleft
  LPath.AddArc(l + w - d, t, d, d, 270, 90); // topright
  LPath.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
  LPath.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft

  LPath.CloseFigure();
  result := LPath;
end;
*)
function GetRoundedCornersPath(ARectangle: TGPRectF;
  ARadius: Single; ARoundedCorners: TRoundedCorners): TGPGraphicsPath;
const
  d0 = 0.0001;
var
  l, t, w, h, d : Single;
begin
  Result := TGPGraphicsPath.Create;
  try
    l := ARectangle.X;
    t := ARectangle.Y;
    w := ARectangle.Width;
    h := ARectangle.Height;
    d := ARadius / 2;
    d := Min(d, Min(ARectangle.Width, ARectangle.Height));
    // topleft
    if rcTopLeft in ARoundedCorners then
      Result.AddArc(l, t, d, d, 180, 90)
    else
      Result.AddArc(l, t, d0, d0, 180, 90);

    // topright
    if rcTopRight in ARoundedCorners then
      Result.AddArc(l + w - d, t, d, d, 270, 90)
    else
      Result.AddArc(l + w - d0, t, d0, d0, 270, 90);

    // bottomright
    if rcBottomRight in ARoundedCorners then
      Result.AddArc(l + w - d, t + h - d, d, d, 0, 90)
    else
      Result.AddArc(l + w - d0, t + h - d0, d0, d0, 0, 90);

    // bottomleft
    if rcBottomLeft in ARoundedCorners then
      Result.AddArc(l, t + h - d, d, d, 90, 90)
    else
      Result.AddArc(l, t + h - d0, d0, d0, 90, 90);

    Result.CloseFigure();
  except
    FreeAndNil(Result);
    raise;
  end
end;

function GPColor(AColor: TColor): TGPColor;
var
  ColRef: COLORREF;
begin
  ColRef := ColorToRGB(AColor);
  Result := MakeColor(GetRValue(ColRef), GetGValue(ColRef),
  GetBValue(ColRef));
end;
{$ifend}

const //Same as Vcl.Buttons
  BitBtnResNames: array[TBitBtnKind] of PChar = (
    nil, 'BBOK', 'BBCANCEL', 'BBHELP', 'BBYES', 'BBNO', 'BBCLOSE',
    'BBABORT', 'BBRETRY', 'BBIGNORE', 'BBALL');

procedure CalcImageAndTextRect(const ASurfaceRect: TRect;
  const ACaption: string;
  out ATextRect: TRect; out AImageRect: TRect;
  const AImageWidth, AImageHeight: Integer;
  const AImageAlignment: TImageAlignment;
  const AImageMargins: TImageMargins;
  const AScale: Single);
var
  IW, IH, IX, IY: Integer;
  LImageAlignment: TImageAlignment;
begin
  if ACaption = '' then
    LImageAlignment := iaCenter
  else
    LImageAlignment := AImageAlignment;
  //Text Rect as whole surface Rect (if there is no Image)
  ATextRect := ASurfaceRect;

  //Calc Image Rect and Change ATextRect
  IH := AImageHeight;
  IW := AImageWidth;
  if (IH > 0) and (IW > 0) then
  begin
    IX := Round(ATextRect.Left + (2*AScale));
    IY := ATextRect.Top + (ATextRect.Height - IH) div 2;
    case LImageAlignment of
      iaCenter:
        begin
          IX := ATextRect.CenterPoint.X - IW div 2;
        end;
      iaLeft:
        begin
          IX := Round(ATextRect.Left + (2*AScale));
          Inc(IX, AImageMargins.Left);
          Inc(IY, AImageMargins.Top);
          Dec(IY, AImageMargins.Bottom);
          ATextRect.Left := IX + IW + AImageMargins.Right;
        end;
      iaRight:
        begin
          IX := Round(ATextRect.Right - IW - (2*AScale));
          Dec(IX, AImageMargins.Right);
          Dec(IX, AImageMargins.Left);
          Inc(IY, AImageMargins.Top);
          Dec(IY, AImageMargins.Bottom);
          ATextRect.Right := IX;
        end;
      iaTop:
        begin
          IX := ATextRect.Left + (ATextRect.Width - IW) div 2;
          Inc(IX, AImageMargins.Left);
          Dec(IX, AImageMargins.Right);
          IY := Round(ATextRect.Top + (2*AScale));
          Inc(IY, AImageMargins.Top);
          ATextRect.Top := IY + IH + AImageMargins.Bottom;
        end;
      iaBottom:
        begin
          IX := ATextRect.Left + (ATextRect.Width - IW) div 2;
          Inc(IX, AImageMargins.Left);
          Dec(IX, AImageMargins.Right);
          IY := Round(ATextRect.Bottom - IH - (2*AScale));
          Dec(IY, AImageMargins.Bottom);
          Dec(IY, AImageMargins.Top);
          ATextRect.Bottom := IY;
        end;
    end;
  end
  else
  begin
    IX := 0;
    IY := 0;
  end;
  AImageRect.Left := IX;
  AImageRect.Top := IY;
  AImageRect.Width := IW;
  AImageRect.Height := IH;

  if ATextRect.IsEmpty then
    ATextRect := ASurfaceRect;
end;

procedure CalcImageAndTextRect(const ACanvas: TCanvas;
  const ACaption: string; const AClient: TRect;
  const AOffset: TPoint;
  var AGlyphPos: TPoint; var ATextBounds: TRect;
  const AImageWidth, AImageHeight: Integer;
  const ALayout: TButtonLayout;
  const AMargin, ASpacing: Integer;
  const ABiDiFlags: Cardinal);
var
  LTextPos: TPoint;
  LClientSize, LGlyphSize, LTextSize: TPoint;
  LTotalSize: TPoint;
  LLayout: TButtonLayout;
  LMargin, LSpacing: Integer;
begin
  LLayout := ALayout;
  if (ABiDiFlags and DT_RIGHT) = DT_RIGHT then
  begin
    if LLayout = blGlyphLeft then LLayout := blGlyphRight
    else if LLayout = blGlyphRight then LLayout := blGlyphLeft;
  end;

  { calculate the item sizes }
  LClientSize := Point(
    AClient.Right - AClient.Left,
    AClient.Bottom - AClient.Top);

  LGlyphSize := Point(AImageWidth, AImageHeight);

  if Length(ACaption) > 0 then
  begin
    ATextBounds := Rect(0, 0, AClient.Right - AClient.Left, 0);
    DrawText(ACanvas.Handle, ACaption, Length(ACaption), ATextBounds,
      DT_CALCRECT or ABiDiFlags);
    LTextSize := Point(
      ATextBounds.Right - ATextBounds.Left,
      ATextBounds.Bottom - ATextBounds.Top);
  end
  else
  begin
    ATextBounds := Rect(0, 0, 0, 0);
    LTextSize := Point(0,0);
  end;

  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if LLayout in [blGlyphLeft, blGlyphRight] then
  begin
    AGlyphPos.Y := (LClientSize.Y - LGlyphSize.Y + 1) div 2;
    LTextPos.Y := (LClientSize.Y - LTextSize.Y + 1) div 2;
  end
  else
  begin
    AGlyphPos.X := (LClientSize.X - LGlyphSize.X + 1) div 2;
    LTextPos.X := (LClientSize.X - LTextSize.X + 1) div 2;
  end;

  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (LTextSize.X = 0) or (LGlyphSize.X = 0) then
    LSpacing := 0
  else
    LSpacing := ASpacing;

  { adjust Margin and Spacing }
  LMargin := AMargin;
  if LMargin = -1 then
  begin
    if LSpacing < 0 then
    begin
      LTotalSize := Point(LGlyphSize.X + LTextSize.X, LGlyphSize.Y + LTextSize.Y);
      if ALayout in [blGlyphLeft, blGlyphRight] then
        LMargin := (LClientSize.X - LTotalSize.X) div 3
      else
        LMargin := (LClientSize.Y - LTotalSize.Y) div 3;
      LSpacing := LMargin;
    end
    else
    begin
      LTotalSize := Point(LGlyphSize.X + LSpacing + LTextSize.X, LGlyphSize.Y +
        LSpacing + LTextSize.Y);
      if LLayout in [blGlyphLeft, blGlyphRight] then
        LMargin := (LClientSize.X - LTotalSize.X + 1) div 2
      else
        LMargin := (LClientSize.Y - LTotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if LSpacing < 0 then
    begin
      LTotalSize := Point(
        LClientSize.X - (LMargin + LGlyphSize.X),
        LClientSize.Y - (LMargin + LGlyphSize.Y));
      if LLayout in [blGlyphLeft, blGlyphRight] then
        LSpacing := (LTotalSize.X - LTextSize.X) div 2
      else
        LSpacing := (LTotalSize.Y - LTextSize.Y) div 2;
    end;
  end;

  case LLayout of
    blGlyphLeft:
      begin
        AGlyphPos.X := LMargin;
        LTextPos.X := AGlyphPos.X + LGlyphSize.X + LSpacing;
      end;
    blGlyphRight:
      begin
        AGlyphPos.X := LClientSize.X - LMargin - LGlyphSize.X;
        LTextPos.X := AGlyphPos.X - LSpacing - LTextSize.X;
      end;
    blGlyphTop:
      begin
        AGlyphPos.Y := LMargin;
        LTextPos.Y := AGlyphPos.Y + LGlyphSize.Y + LSpacing;
      end;
    blGlyphBottom:
      begin
        AGlyphPos.Y := LClientSize.Y - LMargin - LGlyphSize.Y;
        LTextPos.Y := AGlyphPos.Y - LSpacing - LTextSize.Y;
      end;
  end;

  { fixup the result variables }
  Inc(AGlyphPos.X, AClient.Left + AOffset.X);
  Inc(AGlyphPos.Y, AClient.Top + AOffset.Y);

  OffsetRect(ATextBounds, LTextPos.X + AClient.Left + AOffset.X, LTextPos.Y + AClient.Top + AOffset.Y);
end;

procedure DrawIconFromCommandLinkRes(ACanvas: TCanvas; ARect: TRect;
  AVCLStyleName: string; AState: TButtonState; AEnabled: Boolean);
var
  LResName: String;
  LThemeAttribute: TThemeAttribute;
  {$IFDEF D10_4+}
  LImage: TWicImage;
  {$ELSE}
  LBitmap: TBitmap;
  {$ENDIF}
begin
  if AVCLStyleName = RESOURCE_SHIELD_ICON then
  begin
    LResName := RESOURCE_SHIELD_ICON;
  end
  else if (AVCLStyleName = 'Windows') then
  begin
    //Load image from resources by Kind
    LResName := 'CMD_LINK_ARROW_BLUE';
  end
  else
  begin
    if ACanvas.Font.Color = clWhite then
      LResName := 'CMD_LINK_ARROW_WHITE'
    else if ACanvas.Font.Color = clBlack then
      LResName := 'CMD_LINK_ARROW_BLACK'
    else
    begin
      GetStyleAttributes(AVCLStyleName, LThemeAttribute);
      if LThemeAttribute.ThemeType = ttLight then
        LResName := 'CMD_LINK_ARROW_BLACK'
      else
        LResName := 'CMD_LINK_ARROW_WHITE';
    end;
  end;
  {$IFDEF D10_4+}
  LImage := TWicImage.Create;
  try
    LImage.InterpolationMode := wipmHighQualityCubic;
    LImage.LoadFromResourceName(HInstance, LResName);
    ACanvas.StretchDraw(ARect, LImage);
    Exit;
  finally
    LImage.Free;
  end;
  {$ELSE}
  LBitmap := TBitmap.Create;
  try
    LBitmap.PixelFormat := pf32bit;
    //LBitmap.TransparentMode := tmFixed;
    LBitmap.LoadFromResourceName(HInstance, LResName);
    //ACanvas.StretchDraw(ARect, LBitmap);
    //LBitmapRect := TRect.Create(ARect.Top, ARect.Left, LBitmap.Width, LBitmap.Height);
    DrawBitmapTransparent(ACanvas, ARect, ARect.Width, ARect.Height, LBitmap, bsUp, 1, clBlack);
    Exit;
  finally
    LBitmap.Free;
  end;
  {$ENDIF}
end;

procedure DrawBitmapTransparent(ACanvas: TCanvas; ARect: TRect;
  const AWidth, AHeight: Integer; AOriginal: TBitmap;
  AState: TButtonState; ANumGlyphs: Integer; const ATransparentColor: TColor);
const
  ROP_DSPDxax = $00E20746;
var
  IL: TImageList;
  TmpImage, MonoBmp, DDB: TBitmap;
  IRect, ORect: TRect;
  I: TButtonState;
  DestDC: HDC;
  LIndex: Integer;
begin
  LIndex := -1;
  TmpImage := nil;
  IL := nil;
  try
    TmpImage := TBitmap.Create;
    TmpImage.Width := AWidth;
    TmpImage.Height := AHeight;
    IL := TImageList.CreateSize(TmpImage.Width, TmpImage.Height);
    IRect := Rect(0, 0, AWidth, AHeight);
    TmpImage.Canvas.Brush.Color := clBtnFace;
    TmpImage.Palette := CopyPalette(AOriginal.Palette);
    I := AState;
    if Ord(I) >= ANumGlyphs then I := bsUp;
    ORect := Rect(Ord(I) * AWidth, 0, (Ord(I) + 1) * AWidth, AHeight);
    case AState of
      bsUp, bsDown,
      bsExclusive:
        begin
          TmpImage.Canvas.CopyRect(IRect, AOriginal.Canvas, ORect);
          if AOriginal.TransparentMode = tmFixed then
            LIndex := IL.AddMasked(TmpImage, ATransparentColor)
          else
            LIndex := IL.AddMasked(TmpImage, clDefault);
          IL.Masked := True;
        end;
      bsDisabled:
        begin
          MonoBmp := nil;
          DDB := nil;
          try
            MonoBmp := TBitmap.Create;
            DDB := TBitmap.Create;
            DDB.Assign(AOriginal);
            DDB.HandleType := bmDDB;
            if ANumGlyphs > 1 then
            with TmpImage.Canvas do
            begin    { Change white & gray to clBtnHighlight and clBtnShadow }
              CopyRect(IRect, DDB.Canvas, ORect);
              MonoBmp.Monochrome := True;
              MonoBmp.Width := AWidth;
              MonoBmp.Height := AHeight;

              { Convert white to clBtnHighlight }
              DDB.Canvas.Brush.Color := clWhite;
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnHighlight;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, AWidth, AHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert gray to clBtnShadow }
              DDB.Canvas.Brush.Color := clGray;
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnShadow;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, AWidth, AHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert transparent color to clBtnFace }
              DDB.Canvas.Brush.Color := ColorToRGB(ATransparentColor);
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnFace;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, AWidth, AHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
            end
            else
            begin
              { Create a disabled version }
              with MonoBmp do
              begin
                Assign(AOriginal);
                HandleType := bmDDB;
                Canvas.Brush.Color := clBlack;
                Width := AWidth;
                if Monochrome then
                begin
                  Canvas.Font.Color := clWhite;
                  Monochrome := False;
                  Canvas.Brush.Color := clWhite;
                end;
                Monochrome := True;
              end;
              with TmpImage.Canvas do
              begin
                Brush.Color := clBtnFace;
                FillRect(IRect);
                Brush.Color := clBtnHighlight;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 1, 1, AWidth, AHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
                Brush.Color := clBtnShadow;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 0, 0, AWidth, AHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              end;
            end;
          finally
            DDB.Free;
            MonoBmp.Free;
          end;
          LIndex := IL.AddMasked(TmpImage, clDefault);
        end;
    end;
    ImageList_DrawEx(IL.Handle, LIndex, ACanvas.Handle, ARect.Left, ARect.Top, AWidth, AHeight,
      clNone, clNone, ILD_Transparent);
  finally
    IL.Free;
    TmpImage.Free;
  end;
end;

procedure DrawButtonText(const ACanvas: TCanvas;
  const AText: string; const AAlignment: TAlignment;
  const ASpacing: Integer;
  var ARect: TRect; AFlags: Cardinal);
var
  R: TRect;
  OldBKMode: Integer;
begin
  R := ARect;
  Winapi.Windows.DrawText(ACanvas.Handle, PChar(AText), Length(AText),
    R, AFlags or DT_CALCRECT);
  case AAlignment of
    taLeftJustify: OffsetRect(R, ASpacing, (ARect.Height - R.Height) div 2);
    taRightJustify: OffsetRect(R, ARect.Width - R.Width - ASpacing, (ARect.Height - R.Height) div 2);
  else
    OffsetRect(R, (ARect.Width - R.Width) div 2, (ARect.Height - R.Height) div 2);
  end;
  OldBKMode := SetBkMode(ACanvas.Handle, Winapi.Windows.TRANSPARENT);
  CanvasDrawText(ACanvas, R, AText, AFlags);
  SetBkMode(ACanvas.Handle, OldBKMode);
end;

procedure DrawButtonNotificationBadge(const ACanvas: TCanvas;
  const ASurfaceRect: TRect; const AScaleFactor: Single;
  const AValue: string;
  const ASizeType: TNotificationBadgeSize;
  const APosition: TNotificationBadgePosition;
  const AColor, AFontColor: TColor; const AFontStyle: TFontStyles);
var
  LRect: TRect;
  W, H, LBadgeChars, LBadgeBorderSize: Integer;
  LFlags: Cardinal;
begin
  ACanvas.Pen.Style := psClear;
  ACanvas.Brush.Color := AColor;
  ACanvas.Font.Color := AFontColor;
  ACanvas.Font.Style := AFontStyle;

  //Calculate Badge Size
  LFlags := DT_NOCLIP or DT_CENTER or DT_VCENTER or DT_CALCRECT;
  LRect := ASurfaceRect;
  LBadgeChars := Length(AValue);
  Winapi.Windows.DrawText(ACanvas.Handle,
    PChar(AValue), LBadgeChars, LRect, LFlags);

  //Add Border
  LBadgeBorderSize := Round(3 * AScaleFactor);
  InflateRect(LRect, Round(LBadgeBorderSize*2.2), LBadgeBorderSize);
  if ASizeType = nbsSmallDot then
  begin
    //Reduce size of dot based on Font Size
    H := Round(LRect.Height / 2);
    W := H;
  end
  else
  begin
    H := LRect.Height;
    W := Max(LRect.Width, H);
  end;

  //Calculate Badge Position
  if APosition in [nbpTopLeft, nbpTopRight] then
  begin
    LRect.Top := ASurfaceRect.Top;
    LRect.Bottom := LRect.Top + H;
  end
  else
  begin
    LRect.Bottom := ASurfaceRect.Bottom;
    LRect.Top := LRect.Bottom - H;
  end;
  if APosition in [nbpTopRight, nbpBottomRight] then
  begin
    LRect.Right := ASurfaceRect.Right;
    LRect.Left := ASurfaceRect.Right - W;
  end
  else
  begin
    LRect.Left := ASurfaceRect.Left;
    LRect.Right := ASurfaceRect.Left + W;
  end;
  //Draw Badge
  CanvasDrawshape(ACanvas, LRect, btRounded, 0, ALL_ROUNDED_CORNERS, False);

  //Draw Badge Content
  if ASizeType <> nbsSmallDot then
    DrawButtonText(ACanvas, AValue, taCenter, 0, LRect,
      DT_NOCLIP or DT_CENTER or DT_VCENTER);
end;

procedure DrawBitBtnGlyph(ACanvas: TCanvas; ARect: TRect;
  Kind: Vcl.Buttons.TBitBtnKind;
  AState: TButtonState; AEnabled: Boolean;
  AOriginal: TBitmap; ANumGlyphs: Integer; const ATransparentColor: TColor);
var
  LResName: String;
  LOriginal: TBitmap;
  LNumGlyphs: Integer;
  {$IFDEF D10_4+}
  LImage: TWicImage;
  {$ENDIF}
begin
  if not AEnabled then
    AState := bsDisabled;
  LOriginal := nil;
  try
    if Kind = bkCustom then
    begin
      if ANumGlyphs = 0 then
        Exit;
      LOriginal := AOriginal;
      LNumGlyphs := ANumGlyphs;
    end
    else
    begin
      //Load image from resources by Kind
      LResName := BitBtnResNames[Kind];
      {$IFDEF D10_4+}
      LImage := TWicImage.Create;
      try
        LImage.InterpolationMode := wipmHighQualityCubic;
        LImage.LoadFromResourceName(HInstance, LResName);
        ACanvas.StretchDraw(ARect, LImage);
        Exit;
      finally
        LImage.Free;
      end;
      {$ELSE}
        LOriginal := TBitmap.Create;
        LNumGlyphs := 2;
        LOriginal.PixelFormat := pf32bit;
        LOriginal.LoadFromResourceName(HInstance, LResName);
      {$ENDIF}
    end;
    if not Assigned(LOriginal) or ((LOriginal.Width = 0) or (LOriginal.Height = 0)) then
      Exit;
    DrawBitmapTransparent(ACanvas, ARect, LOriginal.Width div LNumGlyphs, LOriginal.Height, LOriginal,
      AState, LNumGlyphs, ATransparentColor);
  finally
    if Kind <> bkCustom then
      LOriginal.Free;
  end;
end;

procedure CanvasDrawBar(const ACanvas: TCanvas; const ARect: TRect;
  const AScaleFactor: Single; ABarColor: TColor);
var
  Points2: array [0..1] of TPoint;
  LRect: TRect;
begin
  //Draw vertical bar
  ACanvas.Pen.Color := ABarColor;
  LRect := Rect(ARect.Left-2,ARect.Top+2,ARect.Right-2,ARect.Bottom-2);
  Points2[0] := Point(ARect.Left -1, ARect.Top + ACanvas.Pen.Width);
  Points2[1] := Point(ARect.Left -1, ARect.Bottom - ACanvas.Pen.Width);
  ACanvas.Polyline(Points2);
end;

procedure CanvasDrawTriangle(const ACanvas: TCanvas; const ARect: TRect;
  const AScaleFactor: Single; ATriangleColor: TColor);
var
  LWidth: Integer;
  LHeight: Integer;
  LMargin: Integer;
  Points3: array [0..2] of TPoint;
  LRect: TRect;
begin
  //Draw triangle
  LHeight := Round(4 * AScaleFactor);
  LMargin := (ARect.Height - LHeight) div 2;
  ACanvas.Pen.Color := ATriangleColor;
  LRect := ARect;
  LWidth  := LRect.Width - 8;
  LRect.Left := ARect.Left + 2;
  LRect.Right := LRect.Left + LWidth - 2;
  LRect.Top := LMargin;
  LRect.Bottom := LRect.Top + LHeight;
  Points3[0] := Point(LRect.Left + LWidth, LRect.Top);
  Points3[1] := Point(LRect.Left, LRect.Top);
  Points3[2] := Point(LRect.Left + LWidth div 2, LRect.Bottom);
  ACanvas.Brush.Color := ACanvas.Pen.Color;
  ACanvas.Pen.Width := 1;
  ACanvas.Polygon(Points3);
end;

procedure CanvasDrawBarAndTriangle(const ACanvas: TCanvas; const ARect: TRect;
  const AScaleFactor: Single; ABarColor, ATriangleColor: TColor);
begin
  //Draw vertical bar
  CanvasDrawBar(ACanvas, ARect, AScaleFactor, ABarColor);

  //Draw triangle
  CanvasDrawTriangle(ACanvas, ARect, AScaleFactor, ATriangleColor);
end;

{$ifdef GDIPlusSupport}
procedure CanvasDrawShape(const ACanvas: TCanvas; ARect: TRect;
  const ADrawType: TStyledButtonDrawType; const ACornerRadius: Single;
  const ARoundedCorners: TRoundedCorners;
  const APreserveBorderSpace: Boolean = True);
var
  LGraphics: TGPGraphics;
  LPen: TGPPen;
  LBrush: TGPBrush;
  LButtonColor, LPenColor: TGPColor;
  LRect: TGPRectF;
  LPath: TGPGraphicsPath;
  LBorderWidth: Single;
  X, Y, W, H: Single;
  LCornerRadius: Single;

  procedure GPInflateRectF(var ARect: TGPRectF;
    const AValue: Single);
  begin
    ARect.X := ARect.X + (AValue / 2);
    ARect.Y := ARect.Y + (AValue / 2);
    ARect.Width := ARect.width - AValue -1;
    ARect.Height := ARect.Height - AValue -1;
  end;

begin
  LGraphics := nil;
  LPen := nil;
  LBrush := nil;
  LPath := nil;
  try
    X := ARect.Left;
    Y := ARect.Top;
    W := ARect.Width;
    H := ARect.Height;
    LRect := Winapi.GDIPAPI.MakeRect(X, Y, W, H);
    LBorderWidth := ACanvas.Pen.Width;
    LGraphics := TGPGraphics.Create(ACanvas.Handle);
    LGraphics.SetSmoothingMode(SmoothingModeAntiAlias);
    LPenColor := GPColor(ACanvas.Pen.Color);
    LButtonColor := GPColor(ACanvas.Brush.Color);

    if ACanvas.Pen.Style = psClear then
      LPen := TGPPen.Create(TAlphaColorRec.Null, LBorderWidth)
    else
      LPen := TGPPen.Create(LPenColor, LBorderWidth);

    if (ADrawType in [btRect]) then
    begin
      DrawRect(ACanvas, ARect);
    end
    else if (ADrawType in [btRounded, btRoundRect]) then
    begin
      //Reduce canvas to draw a rounded rectangle of Pen Width
      if APreserveBorderSpace then
        GPInflateRectF(LRect, LBorderWidth)
      else
        GPInflateRectF(LRect, 1);
      if ADrawType = btRoundRect then
        LCornerRadius := ACornerRadius //Drawing a Rounded Rect
      else
        LCornerRadius := LRect.Height - LBorderWidth; //Drawing a Rounded Button
      LPath := GetRoundedCornersPath(LRect, LCornerRadius*2, ARoundedCorners);
      if ACanvas.Brush.Style = bsSolid then
      begin
        LBrush := TGPSolidBrush.Create(LButtonColor);
        LGraphics.FillPath(LBrush, LPath);
      end;
      LGraphics.DrawPath(LPen, LPath);
    end
    else
    begin
      //Reduce canvas
      GPInflateRectF(LRect, LBorderWidth);
      //Drawing Circle or Ellipsis
      if ACanvas.Brush.Style = bsSolid then
      begin
        LBrush := TGPSolidBrush.Create(LButtonColor);
        LGraphics.FillEllipse(LBrush, LRect);
      end;
      LGraphics.DrawEllipse(LPen, LRect);
    end;
  finally
    LGraphics.Free;
    LPen.Free;
    LBrush.Free;
    LPath.Free;
  end;
end;
{$else}
procedure CanvasDrawShape(const ACanvas: TCanvas; ARect: TRect;
  const ADrawType: TStyledButtonDrawType; const ACornerRadius: Single;
  const APreserveBorderSpace: Boolean = True);
var
  LCornerRadius, LBorderWidth: Integer;
begin
  LBorderWidth := ACanvas.Pen.Width;
  if ADrawType in [btRounded, btRoundRect] then
  begin
    if APreserveBorderSpace then
      AdjustCanvasRect(ACanvas, ARect, False);
    if ADrawType = btRoundRect then
      LCornerRadius := Round(ACornerRadius*2) //Drawing a Rounded Rect
    else
      LCornerRadius := ARect.Height - LBorderWidth; //Drawing a Rounded Button
    ACanvas.RoundRect(ARect, Round(LCornerRadius), LCornerRadius);
  end
  else if ADrawType in [btRect] then
  begin
    AdjustCanvasRect(ACanvas, ARect, True);
    if ACanvas.Brush.Style = bsSolid then
      ACanvas.FillRect(ARect);
    ACanvas.Rectangle(ARect);
  end
  else
  begin
    //Drawing Circle or Ellipsis
    ACanvas.Ellipse(ARect.Left, ARect.Top,
      ARect.Left + ARect.Width, ARect.Top + ARect.Height);
  end;
end;
{$endif}

{$ifdef DrawRectWithGDIPlus}
procedure DrawRect(ACanvas: TCanvas; var ARect: TRect);
var
  LGraphics: TGPGraphics;
  LBrush: TGPBrush;
  LPen: TGPPen;
  LButtonColor, LPenColor: TGPColor;
  LBorderWidth: Integer;
  LRect: TGPRectF;
  X, Y, W, H: Single;

  procedure GPInflateRectF(var ARect: TGPRectF;
    const AValue: Single);
  begin
    ARect.X := ARect.X + (AValue / 2);
    ARect.Y := ARect.Y + (AValue / 2);
    ARect.Width := ARect.width - AValue;
    ARect.Height := ARect.Height - AValue;
  end;

begin
  LGraphics := nil;
  LBrush := nil;
  LPen := nil;
  try
    X := ARect.Left;
    Y := ARect.Top;
    W := ARect.Width;
    H := ARect.Height;
    LGraphics := TGPGraphics.Create(ACanvas.Handle);
    LPenColor := GPColor(ACanvas.Pen.Color);
    LButtonColor := GPColor(ACanvas.Brush.Color);
    LBrush := TGPSolidBrush.Create(LButtonColor);
    LBorderWidth := ACanvas.Pen.Width;
    if ACanvas.Pen.Style = psClear then
      LPen := TGPPen.Create(TAlphaColorRec.Null, LBorderWidth)
    else
      LPen := TGPPen.Create(LPenColor, LBorderWidth);
    //Reduce canvas to draw a rounded rectangle of Pen Width
    LRect := Winapi.GDIPAPI.MakeRect(X, Y, W, H);
    GPInflateRectF(LRect, LBorderWidth);
    //GDI+ equivalent of FillRect and Rectangle
    LGraphics.FillRectangle(LBrush, X, Y, W, H);
    LGraphics.DrawRectangle(LPen, LRect);
  finally
    LBrush.Free;
    LGraphics.Free;
    LPen.Free;
  end;
end;
{$else}
procedure DrawRect(ACanvas: TCanvas; var ARect: TRect);
begin
  //Drawing Rectangular button (no need to GDI+)
  AdjustCanvasRect(ACanvas, ARect, True);
  if ACanvas.Brush.Style = bsSolid then
    ACanvas.FillRect(ARect);
  ACanvas.Rectangle(ARect);
end;
{$endif}


{$ifdef DrawTextWithGDIPlus}
function FontStyleToGDI(AFont: TFont): TFontStyle;
begin
  Result := FontStyleRegular;
  if fsBold in AFont.Style then
    Result := Result + FontStyleBold;
  if fsItalic in AFont.Style then
    Result := Result + FontStyleItalic;
  if fsUnderline in AFont.Style then
    Result := Result + FontStyleUnderline;
  if fsStrikeOut in AFont.Style then
    Result := Result + FontStyleStrikeout;
end;

procedure CanvasDrawText(const ACanvas: TCanvas; ARect: TRect;
  const AText: string; ABiDiModeFlags: LongInt);
var
  LGraphics: TGPGraphics;
  LFontFamily: TGPFontFamily;
  LFont: TGPFont;
  LFontStyle: TFontStyle;
  LSolidBrush: TGPSolidBrush;
  LFontColor: TGPColor;
  LPointF: TGPPointF;
  X,Y: Single;
  R: TRectF;
begin
  LGraphics := nil;
  LFontFamily := nil;
  LFont := nil;
  try
    LGraphics := TGPGraphics.Create(ACanvas.Handle);
    LFontFamily := TGPFontFamily.Create(ACanvas.Font.Name);
    LFontStyle := FontStyleToGDI(ACanvas.Font);
    LFont := TGPFont.Create(LFontFamily, -ACanvas.Font.Height,
      LFontStyle, UnitPixel);
    LFontColor := GPColor(ACanvas.Font.Color);
    LSolidBrush := TGPSolidBrush.Create(LFontColor);
    X := ARect.Left-1;
    Y := ARect.Top-1;
    LPointF := MakePoint(X, Y);
    LGraphics.DrawString(AText, Length(AText), LFont, LPointF, LSolidBrush);
  finally
    LGraphics.Free;
    LFontFamily.Free;
    LFont.Free;
  end;
end;
{$else}
procedure CanvasDrawText(const ACanvas: TCanvas; ARect: TRect;
  const AText: string; ABiDiModeFlags: LongInt);
begin
  Winapi.Windows.DrawText(ACanvas.Handle, PChar(AText),
    Length(AText), ARect, ABiDiModeFlags);
end;
{$endif}

initialization
  _WindowsVersion := wvUndefined;

finalization
  FFamilies.Free;

end.
