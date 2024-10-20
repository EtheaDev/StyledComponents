{******************************************************************************}
{                                                                              }
{   TStyledGraphicButton: a "styled" Button based on TGraphicControl           }
{   TStyledButton: a "styled" Button Component similar to TButton              }
{   TStyledSpeedButton: a "styled" Button Component similar to TSpeedButton    }
{   TStyledBitBtn: a "styled" Button Component similar to TBitBtn              }
{                                                                              }
{   Copyright (c) 2022-2024 (Ethea S.r.l.)                                     }
{   Author: Carlo Barazzetta                                                   }
{   Contributors:                                                              }
{                                                                              }
{   https://github.com/EtheaDev/StyledComponents                               }
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
unit Vcl.StyledButton;

interface

{$INCLUDE StyledComponents.inc}

uses
  System.Math
  , Vcl.ImgList
  , System.UITypes
  , System.SysUtils
  , System.Classes
  , Winapi.Windows
  , Winapi.CommCtrl
  , Winapi.Messages
  , Vcl.Graphics
  , Vcl.buttons
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  , Vcl.Themes
  , Vcl.Controls
  , Vcl.ActnList
  , Vcl.Menus
  , Vcl.ButtonStylesAttributes
  , Vcl.StandardButtonStyles
  ;

resourcestring
  ERROR_SETTING_BUTTON_STYLE = 'Error setting Button Style: %s/%s/%s not available';
  ERROR_CANNOT_USE_RENDER = 'Error: cannot use TStyledButtonRender in this context';

type
  EStyledButtonError = Exception;

  TStyledButtonState = (bsmNormal, bsmPressed, bsmSelected, bsmHot, bsmDisabled);

  TStyledButtonRender = class;
  TStyledButtonRenderClass = class of TStyledButtonRender;

  { TGraphicButtonActionLink }
  TGraphicButtonActionLink = class(TControlActionLink)
  strict private
    function ClientRender: TStyledButtonRender;
    function AssignedClientRender: Boolean;
  strict protected
    FClient: TControl;
    function IsEnabledLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    {$IFDEF D10_4+}
    function IsImageNameLinked: Boolean; override;
    {$ENDIF}
    procedure SetEnabled(Value: Boolean); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure AssignClient(AClient: TObject); override;
    procedure SetGroupIndex(Value: Integer); override;
    procedure SetChecked(Value: Boolean); override;
  public
    function IsCheckedLinked: Boolean; override;
    function IsGlyphLinked(Index: TImageIndex): Boolean; virtual;
  end;

  TControlFont = procedure (var AFont: TFont) of Object;

  TSetCaption = procedure (const ACaption: TCaption) of Object;
  TGetCaption = function : TCaption of Object;

  TSetParentFont = procedure (const AParentFont: Boolean) of Object;
  TGetParentFont = function: Boolean of Object;

  { TStyledButtonRender }
  TStyledButtonRender = class(TObject)
  strict private
    FOwnerControl: TControl;

    FUpdateCount: Integer;
    FRescalingButton: Boolean;
    FButtonStyleNormal: TStyledButtonAttributes;
    FButtonStylePressed: TStyledButtonAttributes;
    FButtonStyleSelected: TStyledButtonAttributes;
    FButtonStyleHot: TStyledButtonAttributes;
    FButtonStyleDisabled: TStyledButtonAttributes;
    FNotificationBadge: TNotificationBadgeAttributes;
    FModalResult: TModalResult;
    FMouseInControl: Boolean;
    FState: TButtonState;
    FImageMargins: TImageMargins;

    FStyleRadius: Integer;
    FStyleRoundedCorners: TRoundedCorners;
    FStyleDrawType: TStyledButtonDrawType;
    FStyleFamily: TStyledButtonFamily;
    FStyleClass: TStyledButtonClass;
    FStyleAppearance: TStyledButtonAppearance;

    FStyleApplied: Boolean;

    FDisabledImages: TCustomImageList;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;

    FDisabledImageIndex: TImageIndex;
    FHotImageIndex: TImageIndex;
    FStylusHotImageIndex: TImageIndex;
    FPressedImageIndex: TImageIndex;
    FSelectedImageIndex: TImageIndex;
    {$IFDEF D10_4+}
    FDisabledImageName: TImageName;
    FHotImageName: TImageName;
    FStylusHotImageName: TImageName;
    FPressedImageName: TImageName;
    FSelectedImageName: TImageName;
    {$ENDIF}
    FSpacing: Integer;
    FMargin: Integer;
    FImageAlignment: TImageAlignment;
    FButtonLayout: TButtonLayout;
    FTag: Integer;
    FWordWrap: Boolean;
    FActive: Boolean;
    FDefault: Boolean;
    FCancel: Boolean;
    FKind: TBitBtnKind;
    FStyle: TCustomButton.TButtonStyle;
    FDropDownMenu: TPopupMenu;
    FDropDownRect: TRect;
    FOnDropDownClick: TNotifyEvent;
    FMouseOverDropDown: Boolean;
    FElevationRequired: Boolean;
    FOnClick: TNotifyEvent;
    FControlFont: TControlFont;
    FSetCaption: TSetCaption;
    FGetCaption: TGetCaption;
    FSetParentFont: TSetParentFont;
    FGetParentFont: TGetParentFont;
    FImageIndex: TImageIndex;
    {$IFDEF D10_4+}
    FImageName: TImageName;
    {$ENDIF}
    FGlyph: TBitmap;
    FNumGlyphs: TNumGlyphs;
    FTransparentColor: TColor;
    FTransparent: Boolean;
    FFlat: Boolean;
    FCaptionAlignment: TAlignment;
    FCommandLinkHint: string;

    FAllowAllUp: Boolean;
    FGroupIndex: Integer;
    FDown: Boolean;
    FShowCaption: Boolean;

    FAutoClick: Boolean;
    FAutoClickDelay: Integer;
    FAutoClickTimer: TTimer;
    FStartAutoClick: TDateTime;
    FAutoClickPixels: Integer;

    procedure SetImageMargins(const AValue: TImageMargins);
    procedure SetStyleRadius(const AValue: Integer);
    procedure SetStyleRoundedCorners(const AValue: TRoundedCorners);
    procedure SetStyleFamily(const AValue: TStyledButtonFamily);
    procedure SetStyleClass(const AValue: TStyledButtonClass);
    procedure SetStyleAppearance(const AValue: TStyledButtonAppearance);
    function ApplyButtonStyle: Boolean;

    procedure SetDisabledImages(const AValue: TCustomImageList);
    procedure SetImages(const AValue: TCustomImageList);

    procedure SetDisabledImageIndex(const AValue: TImageIndex);
    procedure SetHotImageIndex(const AValue: TImageIndex);
    procedure SetStylusHotImageIndex(const AValue: TImageIndex);
    function GetImageIndex: TImageIndex;
    procedure SetImageIndex(const AValue: TImageIndex);
    procedure SetPressedImageIndex(const AValue: TImageIndex);
    procedure SetSelectedImageIndex(const AValue: TImageIndex);

    {$IFDEF D10_4+}
    procedure UpdateImageIndex(Name: TImageName; var Index: TImageIndex);
    procedure UpdateImageName(Index: TImageIndex; var Name: TImageName);
    procedure SetDisabledImageName(const AValue: TImageName);
    procedure SetHotImageName(const AValue: TImageName);
    procedure SetStylusHotImageName(const AValue: TImageName);
    function GetImageName: TImageName;
    procedure SetImageName(const AValue: TImageName);
    procedure SetPressedImageName(const AValue: TImageName);
    procedure SetSelectedImageName(const AValue: TImageName);
    {$ENDIF}

    function GetAttributes(const AMode: TStyledButtonState): TStyledButtonAttributes;
    procedure ImageMarginsChange(Sender: TObject);
    procedure SetImageAlignment(const AValue: TImageAlignment);
    procedure DrawNotificationBadge(
      const ACanvas: TCanvas; const ASurfaceRect: TRect);
    procedure DrawBackgroundAndBorder(const ACanvas: TCanvas;
      const AEraseBackground: Boolean);
    function GetDrawingStyle(const ACanvas: TCanvas;
      const AButtonState: TStyledButtonState): TStyledButtonAttributes;
    procedure SetStyleDrawType(const AValue: TStyledButtonDrawType);
    procedure ImageListChange(Sender: TObject);

    procedure SetButtonStylePressed(const AValue: TStyledButtonAttributes);
    procedure SetButtonStyleSelected(const AValue: TStyledButtonAttributes);
    procedure SetButtonStyleHot(const AValue: TStyledButtonAttributes);
    procedure SetButtonStyleNormal(const AValue: TStyledButtonAttributes);
    procedure SetButtonStyleDisabled(const AValue: TStyledButtonAttributes);
    procedure SetNotificationBadge(const AValue: TNotificationBadgeAttributes);

    procedure SetWordWrap(const AValue: Boolean);
    procedure SetStyleApplied(const AValue: Boolean);
    function GetKind: TBitBtnKind;
    procedure SetKind(const AValue: TBitBtnKind);
    function UpdateStyleUsingModalResult: boolean;
    procedure SetDropDownMenu(const AValue: TPopupMenu);
    procedure SetStyle(const AValue: TCustomButton.TButtonStyle);
    function GetActiveStyleName: string;
    function AsVCLStyle: Boolean;
    function GetAsVCLComponent: Boolean;
    procedure SetAsVCLComponent(const AValue: Boolean);
    //Owner Control access
    function GetAction: TCustomAction;
    procedure SetAction(const AAction: TCustomAction);
    function GetParent: TWinControl;
    procedure SetParent(const AValue: TWinControl);
    function GetControlEnabled: Boolean;
    procedure SetControlEnabled(const AValue: Boolean);
    function GetParentFont: Boolean;
    procedure SetParentFont(const AValue: Boolean);
    function GetFont: TFont;
    function GetComponentState: TComponentState;
    function GetComponentHeight: Integer;
    function GetComponentWidth: Integer;
    function GetHint: string;
    function GetButtonState: TStyledButtonState;
    function GetHandle: HWND;
    procedure CalcDefaultImageMargins(const AValue: TImageAlignment);
    procedure SetGlyph(const AValue: TBitmap);
    function GetGlyph: TBitmap;
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(const AValue: TNumGlyphs);
    procedure SetFlat(const AValue: Boolean);
    procedure SetState(const AValue: TButtonState);
    function GetMouseInControl: Boolean;
    function GetHasCustomAttributes: Boolean;
    procedure SetHasCustomAttributes(const AValue: Boolean);
    procedure SetLayout(const AValue: TButtonLayout);
    procedure SetMargin(const AValue: Integer);
    procedure SetSpacing(const AValue: Integer);
    procedure SetTransparent(const AValue: Boolean);
    procedure SetCaptionAlignment(const AValue: TAlignment);
    procedure SetCommandLinkHint(const AValue: string);
    procedure SetElevationRequired(const AValue: Boolean);
    procedure SetAllowAllUp(const AValue: Boolean);
    procedure SetDown(const AValue: Boolean);
    procedure SetGroupIndex(const AValue: Integer);
    procedure SetShowCaption(const AValue: Boolean);
    procedure UpAllButtons;
    function GetCaptionToDraw: string;
    procedure SetAutoClick(const AValue: Boolean);
    procedure SetAutoClickDelay(const AValue: Integer);
    procedure UpdateAutoClickTimer(const AReset: Boolean);
    procedure AutoClickOnTimer(Sender: TObject);
    function CalcMaxBorderWidth: Integer;
  private
    function IsGlyphAssigned: Boolean;
  protected
    FCustomDrawType: Boolean;
    FUseButtonLayout: Boolean;
    function BitBtnCaptions(Kind: TBitBtnKind): string;
    procedure Invalidate; virtual;
    function GetOwnerScaleFactor: Single;
    function HasTransparentParts: Boolean;
    function IsCaptionAlignmentStored: Boolean;
    function GetBackGroundColor: TColor;
    function IsDefaultImageMargins: Boolean;
    function UpdateCount: Integer;
    procedure SetModalResult(const AValue: TModalResult);
    function IsPressed: Boolean;
    function GetFocused: Boolean;
    procedure UpdateImageIndexAndName;
    {$IFDEF D10_4+}
    procedure UpdateImages;
    procedure CheckImageIndexes;
    {$ENDIF}
    function GetName: TComponentName;
    function CalcImageRect(const ASurfaceRect: TRect;
      const AImageWidth, AImageHeight: Integer): TRect;
    procedure InternalCopyImage(Image: TBitmap; ImageList: TCustomImageList; Index: Integer);
    function GetInternalImage(out AImageList: TCustomImageList;
      out AImageIndex: Integer): Boolean;
  public
    function GetImageSize(out AWidth, AHeight: Integer;
      out AImageList: TCustomImageList; out AImageIndex: Integer): Boolean; virtual;
    function GetRescalingButton: Boolean;
    procedure SetRescalingButton(const AValue: Boolean);
    function GetSplitButtonWidth: Integer;
    procedure ShowDropDownMenu;
    function GetImage(out AImageList: TCustomImageList;
      out AImageIndex: Integer): Boolean;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure Loaded;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean);
    procedure EraseBackground(const ACanvasHandle: HDC);
    procedure DrawButton(const ACanvas: TCanvas;
      const AEraseBackground: Boolean);
    procedure DrawCaptionAndImage(const ACanvas: TCanvas;
      const ASurfaceRect: TRect);
    procedure SetText(const AValue: TCaption);
    function GetText: TCaption;
    function CanDropDownMenu: boolean;
    //Windows messages
    procedure WMKeyDown(var Message: TMessage);
    procedure WMKeyUp(var Message: TMessage);
    procedure CMStyleChanged(var Message: TMessage);
    procedure CMEnter(var Message: TCMEnter);
    procedure CMMouseEnter(var Message: TNotifyEvent);
    procedure CMMouseLeave(var Message: TNotifyEvent);
    procedure CMEnabledChanged(var Message: TMessage);
    {$IFDEF HiDPISupport}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); virtual;
    {$ENDIF}
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsCustomDrawType: Boolean;
    function IsCustomRadius: Boolean;
    function IsStoredStyleFamily: Boolean;
    function IsStoredStyleClass: Boolean;
    function IsStoredStyleAppearance: Boolean;
    function IsStoredStyleElements: Boolean;
    function IsStyleSelectedStored: Boolean;
    function IsStyleHotStored: Boolean;
    function IsStyleNormalStored: Boolean;
    function IsStyleDisabledStored: Boolean;
    function IsStylePressedStored: Boolean;
    function IsNotificationBadgeStored: Boolean;
    procedure SetButtonStyle(const AStyleFamily: TStyledButtonFamily;
      const AStyleClass: TStyledButtonClass;
      const AStyleAppearance: TStyledButtonAppearance); overload;
    procedure SetButtonStyle(const AStyleFamily: TStyledButtonFamily;
      const AModalResult: TModalResult); overload;

    procedure SetFocus;
    procedure AssignStyleTo(ADest: TStyledButtonRender);
    function AssignAttributes(
      const AEnabled: Boolean = True;
      const AImageList: TCustomImageList = nil;
      {$IFDEF D10_4+}const AImageName: string = '';{$ENDIF}
      const AImageIndex: Integer = -1;
      const AImageAlignment: TImageAlignment = iaLeft;
      const AAction: TCustomAction = nil;
      const AOnClick: TNotifyEvent = nil;
      const AName: string = ''): TControl; overload;
    function AssignAttributes(
      const AEnabled: Boolean = True;
      const AImageList: TCustomImageList = nil;
      {$IFDEF D10_4+}const AImageName: string = '';{$ENDIF}
      const AImageIndex: Integer = -1;
      const AButtonLayout: TButtonLayout = blGlyphLeft;
      const AAction: TCustomAction = nil;
      const AOnClick: TNotifyEvent = nil;
      const AName: string = ''): TControl; overload;

    procedure Click(AKeyPressed: Boolean);
    procedure DoDropDownMenu;
    procedure SetButtonStyles(
      const AFamily: TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance);

    constructor CreateStyled(AOwner: TControl;
      const AOnClick: TNotifyEvent;
      const AControlFont: TControlFont;
      const AGetCaption: TGetCaption;
      const ASetCaption: TSetCaption;
      const AGetParentFont: TGetParentFont;
      const ASetParentFont: TSetParentFont;
      const AFamily: TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance;
      const ADrawType: TStyledButtonDrawType;
      const ACursor: TCursor;
      const AUseCustomDrawType: Boolean); virtual;
    constructor Create(AOwner: TControl;
      const AOnClick: TNotifyEvent;
      const AControlFont: TControlFont;
      const AGetCaption: TGetCaption;
      const ASetCaption: TSetCaption;
      const AGetParentFont: TGetParentFont;
      const ASetParentFont: TSetParentFont);
    destructor Destroy; override;
    function IsDefaultAppearance: Boolean;
    property AsVCLComponent: Boolean read GetAsVCLComponent write SetAsVCLComponent;
    property Active: Boolean read FActive write FActive;
    property AutoClick: Boolean read FAutoClick write SetAutoClick default False;
    property AutoClickDelay: Integer read FAutoClickDelay write SetAutoClickDelay default DEFAULT_AUTOCLICK_DELAY;
    property Focused: Boolean read GetFocused;
    property ButtonState: TStyledButtonState read GetButtonState;
    property StyleApplied: Boolean read FStyleApplied write SetStyleApplied;
    property Caption: TCaption read GetText write SetText;
    property CaptionAlignment: TAlignment read FCaptionAlignment write SetCaptionAlignment;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property CommandLinkHint: string read FCommandLinkHint write SetCommandLinkHint;
    property Default: Boolean read FDefault write FDefault;
    property ElevationRequired: Boolean read FElevationRequired write SetElevationRequired;
    property Cancel: Boolean read FCancel write FCancel;
    property ActiveStyleName: string read GetActiveStyleName;
    property ImageAlignment: TImageAlignment read FImageAlignment write SetImageAlignment;
    property DisabledImageIndex: TImageIndex read FDisabledImageIndex write SetDisabledImageIndex;
    property DisabledImages: TCustomImageList read FDisabledImages write SetDisabledImages;
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    property Flat: Boolean read FFlat write SetFlat;
    property SplitButtonWidth: Integer read GetSplitButtonWidth;
    property HotImageIndex: TImageIndex read FHotImageIndex write SetHotImageIndex;
    property StylusHotImageIndex: TImageIndex read FStylusHotImageIndex write SetStylusHotImageIndex;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex;
    property Kind: TBitBtnKind read GetKind write SetKind default bkCustom;
    property OwnerControl: TControl read FOwnerControl;
    property PressedImageIndex: TImageIndex read FPressedImageIndex write SetPressedImageIndex;
    property SelectedImageIndex: TImageIndex read FSelectedImageIndex write SetSelectedImageIndex;
    property Transparent: Boolean read FTransparent write SetTransparent;
    {$IFDEF D10_4+}
    property DisabledImageName: TImageName read FDisabledImageName write SetDisabledImageName;
    property HotImageName: TImageName read FHotImageName write SetHotImageName;
    property StylusHotImageName: TImageName read FStylusHotImageName write SetStylusHotImageName;
    property ImageName: TImageName read GetImageName write SetImageName;
    property PressedImageName: TImageName read FPressedImageName write SetPressedImageName;
    property SelectedImageName: TImageName read FSelectedImageName write SetSelectedImageName;
    {$ENDIF}
    property ImageMargins: TImageMargins read FImageMargins write SetImageMargins;
    property ModalResult: TModalResult read FModalResult write SetModalResult;
    property RescalingButton: Boolean read GetRescalingButton write SetRescalingButton;

    //Properties used when UseButtonLayout is True
    property Layout: TButtonLayout read FButtonLayout write SetLayout;
    property Margin: Integer read FMargin write SetMargin default -1;
    property Spacing: Integer read FSpacing write SetSpacing default 0;

    //Property used by TStyledButton, TStyledGraphicButton and TStyledSpeedButton
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex;
    property Down: Boolean read FDown write SetDown;

    //Style as TButton
    property Style: TCustomButton.TButtonStyle read FStyle write SetStyle;

    //StyledComponents Attributes
    property StyleRadius: Integer read FStyleRadius write SetStyleRadius;
    property StyleRoundedCorners: TRoundedCorners read FStyleRoundedCorners write SetStyleRoundedCorners default ALL_ROUNDED_CORNERS;
    property StyleDrawType: TStyledButtonDrawType read FStyleDrawType write SetStyleDrawType;
    property StyleFamily: TStyledButtonFamily read FStyleFamily write SetStyleFamily;
    property StyleClass: TStyledButtonClass read FStyleClass write SetStyleClass;
    property StyleAppearance: TStyledButtonAppearance read FStyleAppearance write SetStyleAppearance;

    property Tag: Integer read FTag write FTag;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;

    property ButtonStyleNormal: TStyledButtonAttributes read FButtonStyleNormal write SetButtonStyleNormal;
    property ButtonStylePressed: TStyledButtonAttributes read FButtonStylePressed write SetButtonStylePressed;
    property ButtonStyleSelected: TStyledButtonAttributes read FButtonStyleSelected write SetButtonStyleSelected;
    property ButtonStyleHot: TStyledButtonAttributes read FButtonStyleHot write SetButtonStyleHot;
    property ButtonStyleDisabled: TStyledButtonAttributes read FButtonStyleDisabled write SetButtonStyleDisabled;
    property NotificationBadge: TNotificationBadgeAttributes read FNotificationBadge write SetNotificationBadge;

    property OnDropDownClick: TNotifyEvent read FOnDropDownClick write FOnDropDownClick;

    //Render property accessible from Components
    property State: TButtonState read FState write SetState;
    property MouseInControl: Boolean read GetMouseInControl;

    //Owner Control property Access
    //property Canvas: TCanvas read GetControlCanvas;
    property Enabled: Boolean read GetControlEnabled write SetControlEnabled;
    property ParentFont: Boolean read GetParentFont write SetParentFont;
    property Font: TFont read GetFont;
    property Action: TCustomAction read GetAction write SetAction;
    property Name: TComponentName read GetName; // write SetName;
    property Parent: TWinControl read GetParent write SetParent;
    property ComponentState: TComponentState read GetComponentState;
    property Height: Integer read GetComponentHeight;
    property Width: Integer read GetComponentWidth;
    property Hint: string read GetHint;

    //Owner Control must assign those event-handlers
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property ControlFont: TControlFont read FControlFont write FControlFont;
  public
    procedure SetCustomStyleDrawType(ACustomStyleDrawType: Boolean);
    property HasCustomAttributes: Boolean read GetHasCustomAttributes write SetHasCustomAttributes default False;
  end;

  TCustomStyledGraphicButton = class;
  TCustomStyledButton = class;
  TStyledSpeedButton = class;
  TStyledBitBtn = class;

  { TCustomStyledGraphicButton }
  TCustomStyledGraphicButton = class(TGraphicControl)
  private
    FRender: TStyledButtonRender;
    FImageIndex: TImageIndex;
    {$IFDEF D10_4+}
    FImageName: TImageName;
    {$ENDIF}

    class var
    _DefaultStyleDrawType: TStyledButtonDrawType;
    _UseCustomDrawType: Boolean;
    _DefaultFamily: TStyledButtonFamily;
    _DefaultClass: TStyledButtonClass;
    _DefaultAppearance: TStyledButtonAppearance;
    _DefaultStyleRadius: Integer;
    _DefaultCursor: TCursor;

    //Event Handlers passed to Render
    procedure ControlFont(var AValue: TFont);
    procedure ControlClick(Sender: TObject);
    function GetParentFont: Boolean;
    procedure SetParentfont(const AValue: Boolean);
    function GetImageMargins: TImageMargins;
    procedure SetImageMargins(const AValue: TImageMargins);
    function IsCustomRadius: Boolean;
    function GetStyleRadius: Integer;
    procedure SetStyleRadius(const AValue: Integer);
    function GetStyleRoundedCorners: TRoundedCorners;
    procedure SetStyleRoundedCorners(const AValue: TRoundedCorners);
    function ImageMarginsStored: Boolean;
    function IsStoredStyleFamily: Boolean;
    function IsStoredStyleAppearance: Boolean;
    function IsStoredStyleElements: Boolean;
    function GetStyleFamily: TStyledButtonFamily;
    procedure SetStyleFamily(const AValue: TStyledButtonFamily);
    function GetStyleClass: TStyledButtonClass;
    procedure SetStyleClass(const AValue: TStyledButtonClass);
    function GetStyleAppearance: TStyledButtonAppearance;
    procedure SetStyleAppearance(const AValue: TStyledButtonAppearance);
    function GetTag: Integer;
    procedure SetTag(const AValue: Integer);
    function GetDisabledImages: TCustomImageList;
    procedure SetDisabledImages(const AValue: TCustomImageList);
    function GetImages: TCustomImageList;
    procedure SetImages(const AValue: TCustomImageList);
    function GetDisabledImageIndex: TImageIndex;
    procedure SetDisabledImageIndex(const AValue: TImageIndex);
    function GetHotImageIndex: TImageIndex;
    procedure SetHotImageIndex(const AValue: TImageIndex);
    procedure SetImageIndex(const AValue: TImageIndex);
    function GetPressedImageIndex: TImageIndex;
    procedure SetPressedImageIndex(const AValue: TImageIndex);
    function GetSelectedImageIndex: TImageIndex;
    procedure SetSelectedImageIndex(const AValue: TImageIndex);
    {$IFDEF D10_4+}
    function GetDisabledImageName: TImageName;
    procedure SetDisabledImageName(const AValue: TImageName);
    function GetHotImageName: TImageName;
    procedure SetHotImageName(const AValue: TImageName);
    function GetImageName: TImageName;
    procedure SetImageName(const AValue: TImageName);
    function GetPressedImageName: TImageName;
    procedure SetPressedImageName(const AValue: TImageName);
    function GetSelectedImageName: TImageName;
    procedure SetSelectedImageName(const AValue: TImageName);
    {$ENDIF}
    function GetImageAlignment: TImageAlignment;
    procedure SetImageAlignment(const AValue: TImageAlignment);
    function IsCustomDrawType: Boolean;
    function GetStyleDrawType: TStyledButtonDrawType;
    procedure SetStyleDrawType(const AValue: TStyledButtonDrawType);
    procedure SetText(const AValue: TCaption);
    function GetButtonStylePressed: TStyledButtonAttributes;
    procedure SetButtonStylePressed(const AValue: TStyledButtonAttributes);
    function GetButtonStyleSelected: TStyledButtonAttributes;
    procedure SetButtonStyleSelected(const AValue: TStyledButtonAttributes);
    function GetButtonStyleHot: TStyledButtonAttributes;
    procedure SetButtonStyleHot(const AValue: TStyledButtonAttributes);
    function GetButtonStyleNormal: TStyledButtonAttributes;
    procedure SetButtonStyleNormal(const AValue: TStyledButtonAttributes);
    function GetButtonStyleDisabled: TStyledButtonAttributes;
    procedure SetButtonStyleDisabled(const AValue: TStyledButtonAttributes);
    function GetOnDropDownClick: TNotifyEvent;
    procedure SetOnDropDownClick(const AValue: TNotifyEvent);
    function IsStyleSelectedStored: Boolean;
    function IsStyleHotStored: Boolean;
    function IsStyleNormalStored: Boolean;
    function IsStyleDisabledStored: Boolean;
    function IsStylePressedStored: Boolean;
    function IsNotificationBadgeStored: Boolean;
    function IsImageIndexStored: Boolean;
    {$IFDEF D10_4+}
    function IsImageNameStored: Boolean;
    {$ENDIF}
    function GetWordWrap: Boolean;
    procedure SetWordWrap(const AValue: Boolean);
    function GetStyleApplied: Boolean;
    procedure SetStyleApplied(const AValue: Boolean);
    function GetKind: TBitBtnKind;
    procedure SetKind(const AValue: TBitBtnKind);
    function GetDropDownMenu: TPopupMenu;
    procedure SetDropDownMenu(const AValue: TPopupMenu);
    function GetStyle: TCustomButton.TButtonStyle;
    procedure SetStyle(const AValue: TCustomButton.TButtonStyle);
    function GetFocused: Boolean;
    //Windows messages
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMMouseEnter(var Message: TNotifyEvent); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TNotifyEvent); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;

    function GetModalResult: TModalResult;
    procedure SetModalResult(const AValue: TModalResult);
    function GetFlat: Boolean;
    procedure SetFlat(const AValue: Boolean);

    //Glyph Support
    function HasCustomGlyph: Boolean;
    function GetGlyph: TBitmap;
    procedure SetGlyph(const AValue: TBitmap);
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(const AValue: TNumGlyphs);
    function GetMouseInControl: Boolean;
    function GetCursor: TCursor;
    function GetTransparent: Boolean;
    procedure SetTransparent(const AValue: Boolean);
    function GetCaptionAlignment: TAlignment;
    procedure SetCaptionAlignment(const AValue: TAlignment);
    function GetCommandLinkHint: string;
    procedure SetCommandLinkHint(const AValue: string);
    function IsCaptionAlignmentStored: Boolean;
    function GetSpacing: Integer;
    procedure SetSpacing(const AValue: Integer);
    function GetLayout: TButtonLayout;
    procedure SetLayout(const AValue: TButtonLayout);
    function GetMargin: Integer;
    procedure SetMargin(const AValue: Integer);
    function GetAllowAllUp: Boolean;
    function GetDown: Boolean;
    function GetGroupIndex: Integer;
    procedure SetAllowAllUp(const AValue: Boolean);
    procedure SetDown(const AValue: Boolean);
    procedure SetGroupIndex(const AValue: Integer);
    function IsCheckedStored: Boolean;
    function GetAsVCLComponent: Boolean;
    procedure SetAsVCLComponent(const AValue: Boolean);
    function GetActiveStyleName: string;
    function GetNotificationBadge: TNotificationBadgeAttributes;
    procedure SetNotificationBadge(const AValue: TNotificationBadgeAttributes);
    function GetShowCaption: Boolean;
    procedure SetShowCaption(const AValue: Boolean);
    function GetAutoClick: Boolean;
    function GetAutoClickDelay: Integer;
    procedure SetAutoClick(const AValue: Boolean);
    procedure SetAutoClickDelay(const AValue: Integer);
  protected
    procedure SetCursor(const AValue: TCursor); virtual;
    function GetCaption: TCaption;
    function GetCaptionToDraw: TCaption; virtual;
    procedure SetCaption(const AValue: TCaption); virtual;
    function GetButtonState: TStyledButtonState; virtual;
    function GetImage(out AImageList: TCustomImageList;
      out AImageIndex: Integer): Boolean; virtual;
    function GetText: TCaption; virtual;
    function IsStoredStyleClass: Boolean; virtual;
    function IsEnabledStored: Boolean; virtual;
    function IsCaptionStored: Boolean; virtual;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure SetName(const AValue: TComponentName); override;
    procedure Loaded; override;
    procedure Paint; override;
    {$IFDEF HiDPISupport}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ENDIF}
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    {$IFDEF D10_4+}
    procedure SetStyleName(const AValue: string); override;
    {$ENDIF}
    function GetRenderClass: TStyledButtonRenderClass; virtual;
  public
    class procedure RegisterDefaultRenderingStyle(
      const ADrawType: TStyledButtonDrawType;
      const AFamily: TStyledButtonFamily = DEFAULT_CLASSIC_FAMILY;
      const AClass: TStyledButtonClass = DEFAULT_WINDOWS_CLASS;
      const AAppearance: TStyledButtonAppearance = DEFAULT_APPEARANCE;
      const AStyleRadius: Integer = DEFAULT_RADIUS;
      const ACursor: TCursor = DEFAULT_CURSOR); virtual;
    function GetRescalingButton: Boolean;
    procedure SetRescalingButton(const AValue: Boolean);
    function GetSplitButtonWidth: Integer;
    procedure ShowDropDownMenu;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SetButtonStyle(const AStyleFamily: TStyledButtonFamily;
      const AStyleClass: TStyledButtonClass;
      const AStyleAppearance: TStyledButtonAppearance); overload;
    procedure SetButtonStyle(const AStyleFamily: TStyledButtonFamily;
      const AModalResult: TModalResult); overload;
    procedure AssignStyleTo(ADestRender: TStyledButtonRender); overload;
    procedure AssignStyleTo(ADest: TCustomStyledGraphicButton); overload;
    procedure AssignTo(ADest: TPersistent); override;
    function AssignAttributes(
      const AEnabled: Boolean = True;
      const AImageList: TCustomImageList = nil;
      {$IFDEF D10_4+}const AImageName: string = '';{$ENDIF}
      const AImageIndex: Integer = -1;
      const AImageAlignment: TImageAlignment = iaLeft;
      const AAction: TCustomAction = nil;
      const AOnClick: TNotifyEvent = nil;
      const AName: string = ''): TCustomStyledGraphicButton;
    procedure Click; override;
    procedure DoDropDownMenu;
    constructor CreateStyled(AOwner: TComponent;
      const AFamily: TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance); overload; virtual;
    constructor CreateStyled(AOwner: TComponent;
      const AFamily: TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance;
      const ADrawType: TStyledButtonDrawType;
      const ACursor: TCursor;
      const AUseCustomDrawType: Boolean); overload; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ActiveStyleName: string read GetActiveStyleName;
    property AsVCLComponent: Boolean read GetAsVCLComponent write SetAsVCLComponent;
    property AutoClick: Boolean read GetAutoClick write SetAutoClick default False;
    property AutoClickDelay: Integer read GetAutoClickDelay write SetAutoClickDelay default DEFAULT_AUTOCLICK_DELAY;
    property Focused: Boolean read GetFocused;
    property ButtonState: TStyledButtonState read GetButtonState;
    property MouseInControl: Boolean read GetMouseInControl;
    property Render: TStyledButtonRender read FRender;
    property StyleApplied: Boolean read GetStyleApplied write SetStyleApplied;
    property RescalingButton: Boolean read GetRescalingButton write SetRescalingButton;
    property Enabled stored IsEnabledStored;
    property ParentFont default True;
    property Layout: TButtonLayout read GetLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read GetMargin write SetMargin default -1;
    property Spacing: Integer read GetSpacing write SetSpacing default 0;
    property StyleElements stored IsStoredStyleElements;
    property Caption: TCaption read GetText write SetText stored IsCaptionStored;
    property CaptionAlignment: TAlignment read GetCaptionAlignment write SetCaptionAlignment Stored IsCaptionAlignmentStored;
    property ShowCaption: Boolean read GetShowCaption write SetShowCaption default True;
    property CommandLinkHint: string read GetCommandLinkHint write SetCommandLinkHint;
    property Cursor: TCursor read GetCursor write SetCursor default DEFAULT_CURSOR;
    property ImageAlignment: TImageAlignment read GetImageAlignment write SetImageAlignment default iaLeft;
    property DisabledImageIndex: TImageIndex read GetDisabledImageIndex write SetDisabledImageIndex default -1;
    property DisabledImages: TCustomImageList read GetDisabledImages write SetDisabledImages;
    property DropDownMenu: TPopupMenu read GetDropDownMenu write SetDropDownMenu;
    property Flat: Boolean read GetFlat write SetFlat default False;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored HasCustomGlyph;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property HotImageIndex: TImageIndex read GetHotImageIndex write SetHotImageIndex default -1;
    property Images: TCustomImageList read GetImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex stored IsImageIndexStored;
    property Kind: TBitBtnKind read GetKind write SetKind default bkCustom;
    property PressedImageIndex: TImageIndex read GetPressedImageIndex write SetPressedImageIndex default -1;
    property SelectedImageIndex: TImageIndex read GetSelectedImageIndex write SetSelectedImageIndex default -1;
    {$IFDEF D10_4+}
    property DisabledImageName: TImageName read GetDisabledImageName write SetDisabledImageName;
    property HotImageName: TImageName read GetHotImageName write SetHotImageName;
    property ImageName: TImageName read GetImageName write SetImageName stored IsImageNameStored;
    property PressedImageName: TImageName read GetPressedImageName write SetPressedImageName;
    property SelectedImageName: TImageName read GetSelectedImageName write SetSelectedImageName;
    {$ENDIF}
    property ImageMargins: TImageMargins read GetImageMargins write SetImageMargins stored ImageMarginsStored;
    property ModalResult: TModalResult read GetModalResult write SetModalResult default mrNone;
    //Style as TSpeedButton
    property Style: TCustomButton.TButtonStyle read GetStyle write SetStyle default TCustomButton.TButtonStyle.bsPushButton;
    //Property used by TStyledButton, TStyledGraphicButton and TStyledSpeedButton
    property AllowAllUp: Boolean read GetAllowAllUp write SetAllowAllUp default False;
    property GroupIndex: Integer read GetGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read GetDown write SetDown stored IsCheckedStored default False;
    //StyledComponents Attributes
    property StyleRadius: Integer read GetStyleRadius write SetStyleRadius stored IsCustomRadius default DEFAULT_RADIUS;
    property StyleRoundedCorners: TRoundedCorners read GetStyleRoundedCorners write SetStyleRoundedCorners default ALL_ROUNDED_CORNERS;
    property StyleDrawType: TStyledButtonDrawType read GetStyleDrawType write SetStyleDrawType stored IsCustomDrawType;
    property StyleFamily: TStyledButtonFamily read GetStyleFamily write SetStyleFamily stored IsStoredStyleFamily;
    property StyleClass: TStyledButtonClass read GetStyleClass write SetStyleClass stored IsStoredStyleClass;
    property StyleAppearance: TStyledButtonAppearance read GetStyleAppearance write SetStyleAppearance stored IsStoredStyleAppearance;
    property Tag: Integer read GetTag write SetTag default 0;
    property Transparent: Boolean read GetTransparent write SetTransparent default False;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default False;
    property ButtonStyleNormal: TStyledButtonAttributes read GetButtonStyleNormal write SetButtonStyleNormal stored IsStyleNormalStored;
    property ButtonStylePressed: TStyledButtonAttributes read GetButtonStylePressed write SetButtonStylePressed stored IsStylePressedStored;
    property ButtonStyleSelected: TStyledButtonAttributes read GetButtonStyleSelected write SetButtonStyleSelected stored IsStyleSelectedStored;
    property ButtonStyleHot: TStyledButtonAttributes read GetButtonStyleHot write SetButtonStyleHot stored IsStyleHotStored;
    property ButtonStyleDisabled: TStyledButtonAttributes read GetButtonStyleDisabled write SetButtonStyleDisabled stored IsStyleDisabledStored;
    property NotificationBadge: TNotificationBadgeAttributes read GetNotificationBadge write SetNotificationBadge stored IsNotificationBadgeStored;
    property OnDropDownClick: TNotifyEvent read GetOnDropDownClick write SetOnDropDownClick;
  end;

  { TStyledGraphicButton }
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TStyledGraphicButton = class(TCustomStyledGraphicButton)
  published
    property ActiveStyleName;
    property Action;
    property Align;
    property AllowAllUp;
    property Anchors;
    property AsVCLComponent stored False;
    property AutoClick;
    property AutoClickDelay;
    property Constraints;
    property Cursor default DEFAULT_CURSOR;
    property GroupIndex;
    property Down;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property NotificationBadge;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnGesture;
    property OnStartDock;
    property OnStartDrag;
    property OnClick;
    property PopUpMenu;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    {$IFDEF D10_4+}
    property StyleName;
    {$ENDIF}
    property StyleElements;
    property Transparent;
    property Visible;
    property Caption;
    property CaptionAlignment;
    property ShowCaption;
    property CommandLinkHint;
    property ImageAlignment;
    property DisabledImageIndex;
    property DisabledImages;
    property DropDownMenu;
    property Flat;
    property Glyph;
    property NumGlyphs;
    property HotImageIndex;
    property Images;
    property ImageIndex;
    property Kind;
    property PressedImageIndex;
    property SelectedImageIndex;
    {$IFDEF D10_4+}
    property DisabledImageName;
    property HotImageName;
    property ImageName;
    property PressedImageName;
    property SelectedImageName;
    {$ENDIF}
    property ImageMargins;
    property ModalResult;
    property Style;
    property Tag;
    //StyledComponents Attributes
    property StyleRadius;
    property StyleRoundedCorners;
    property StyleDrawType;
    property StyleFamily;
    property StyleClass;
    property StyleAppearance;
    property WordWrap;
    property ButtonStyleNormal;
    property ButtonStylePressed;
    property ButtonStyleSelected;
    property ButtonStyleHot;
    property ButtonStyleDisabled;
    property OnDropDownClick;
  end;

  { TStyledSpeedButton }
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TStyledSpeedButton = class(TCustomStyledGraphicButton)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Action;
    property Align;
    property AllowAllUp;
    property Anchors;
    property AsVCLComponent stored False;
    property AutoClick;
    property AutoClickDelay;
    property BiDiMode;
    property Constraints;
    property Cursor default DEFAULT_CURSOR;
    property GroupIndex;
    property Down;
    property DisabledImageIndex;
    {$IFDEF D10_4+}
    property DisabledImageName;
    {$ENDIF}
    property Caption;
    property ShowCaption;
    property Enabled;
    {$IFDEF D10_4+}
    property HotImageIndex;
    property HotImageName;
    property ImageIndex;
    property ImageName;
    property Images;
    {$ENDIF}
    property Flat;
    property Font;
    property Glyph;
    property Layout;
    property Margin;
    property NotificationBadge;
    property NumGlyphs;
    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property PopupMenu;
    property PressedImageIndex;
    {$IFDEF D10_4+}
    property PressedImageName;
    {$ENDIF}
    property ShowHint;
    property SelectedImageIndex;
    {$IFDEF D10_4+}
    property SelectedImageName;
    {$ENDIF}
    property Spacing default 4;
    property Transparent default True;
    property Visible;
    property StyleElements;
    {$IFDEF D10_4+}
    property StyleName;
    {$ENDIF}
    property Tag;
    property OnClick;
    property OnDblClick;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    //StyledComponents Attributes
    property StyleRadius;
    property StyleRoundedCorners;
    property StyleDrawType;
    property StyleFamily;
    property StyleClass;
    property StyleAppearance;
    property ButtonStyleNormal;
    property ButtonStylePressed;
    property ButtonStyleSelected;
    property ButtonStyleHot;
    property ButtonStyleDisabled;
  end;

  { TCustomStyledButton }
  TCustomStyledButton = class(TCustomControl)
  private
    FPaintBuffer: TBitmap;
    FPaintBufferUsers: Integer;
    FRender: TStyledButtonRender;
    FImageIndex: TImageIndex;
    {$IFDEF D10_4+}
    FImageName: TImageName;
    {$ENDIF}
    class var
    _DefaultStyleDrawType: TStyledButtonDrawType;
    _UseCustomDrawType: Boolean;
    _DefaultFamily: TStyledButtonFamily;
    _DefaultClass: TStyledButtonClass;
    _DefaultAppearance: TStyledButtonAppearance;
    _DefaultStyleRadius: Integer;
    _DefaultCursor: TCursor;

    //Event Handlers passed to Render
    procedure ControlFont(var AValue: TFont);
    procedure ControlClick(Sender: TObject);
    function GetParentFont: Boolean;
    procedure SetParentfont(const AValue: Boolean);
    function GetImageMargins: TImageMargins;
    procedure SetImageMargins(const AValue: TImageMargins);
    function IsCustomRadius: Boolean;
    function GetStyleRadius: Integer;
    procedure SetStyleRadius(const AValue: Integer);
    function GetStyleRoundedCorners: TRoundedCorners;
    procedure SetStyleRoundedCorners(const AValue: TRoundedCorners);
    function ImageMarginsStored: Boolean;
    function IsStoredStyleFamily: Boolean;
    function IsStoredStyleAppearance: Boolean;
    function IsStoredStyleElements: Boolean;
    function GetStyleFamily: TStyledButtonFamily;
    procedure SetStyleFamily(const AValue: TStyledButtonFamily);
    function GetStyleClass: TStyledButtonClass;
    procedure SetStyleClass(const AValue: TStyledButtonClass);
    function GetStyleAppearance: TStyledButtonAppearance;
    procedure SetStyleAppearance(const AValue: TStyledButtonAppearance);
    function GetTag: Integer;
    procedure SetTag(const AValue: Integer);
    function GetDisabledImages: TCustomImageList;
    procedure SetDisabledImages(const AValue: TCustomImageList);
    function GetImages: TCustomImageList;
    procedure SetImages(const AValue: TCustomImageList);
    function GetDisabledImageIndex: TImageIndex;
    procedure SetDisabledImageIndex(const AValue: TImageIndex);
    function GetHotImageIndex: TImageIndex;
    procedure SetHotImageIndex(const AValue: TImageIndex);
    function GetStylusHotImageIndex: TImageIndex;
    procedure SetStylusHotImageIndex(const AValue: TImageIndex);
    procedure SetImageIndex(const AValue: TImageIndex);
    function GetPressedImageIndex: TImageIndex;
    procedure SetPressedImageIndex(const AValue: TImageIndex);
    function GetSelectedImageIndex: TImageIndex;
    procedure SetSelectedImageIndex(const AValue: TImageIndex);
    {$IFDEF D10_4+}
    function GetDisabledImageName: TImageName;
    procedure SetDisabledImageName(const AValue: TImageName);
    function GetHotImageName: TImageName;
    procedure SetHotImageName(const AValue: TImageName);
    function GetStylusHotImageName: TImageName;
    procedure SetStylusHotImageName(const AValue: TImageName);
    function GetImageName: TImageName;
    procedure SetImageName(const AValue: TImageName);
    function GetPressedImageName: TImageName;
    procedure SetPressedImageName(const AValue: TImageName);
    function GetSelectedImageName: TImageName;
    procedure SetSelectedImageName(const AValue: TImageName);
    {$ENDIF}
    function GetImageAlignment: TImageAlignment;
    procedure SetImageAlignment(const AValue: TImageAlignment);
    function IsCustomDrawType: Boolean;
    function GetStyleDrawType: TStyledButtonDrawType;
    procedure SetStyleDrawType(const AValue: TStyledButtonDrawType);
    procedure SetText(const AValue: TCaption);
    function GetButtonStylePressed: TStyledButtonAttributes;
    procedure SetButtonStylePressed(const AValue: TStyledButtonAttributes);
    function GetButtonStyleSelected: TStyledButtonAttributes;
    procedure SetButtonStyleSelected(const AValue: TStyledButtonAttributes);
    function GetButtonStyleHot: TStyledButtonAttributes;
    procedure SetButtonStyleHot(const AValue: TStyledButtonAttributes);
    function GetButtonStyleNormal: TStyledButtonAttributes;
    procedure SetButtonStyleNormal(const AValue: TStyledButtonAttributes);
    function GetButtonStyleDisabled: TStyledButtonAttributes;
    procedure SetButtonStyleDisabled(const AValue: TStyledButtonAttributes);
    function GetOnDropDownClick: TNotifyEvent;
    procedure SetOnDropDownClick(const AValue: TNotifyEvent);
    function IsStyleSelectedStored: Boolean;
    function IsStyleHotStored: Boolean;
    function IsStyleNormalStored: Boolean;
    function IsStyleDisabledStored: Boolean;
    function IsStylePressedStored: Boolean;
    function IsNotificationBadgeStored: Boolean;
    function IsImageIndexStored: Boolean;
    {$IFDEF D10_4+}
    function IsImageNameStored: Boolean;
    {$ENDIF}
    function GetWordWrap: Boolean;
    procedure SetWordWrap(const AValue: Boolean);
    function GetStyleApplied: Boolean;
    procedure SetStyleApplied(const AValue: Boolean);
    function GetDefault: Boolean;
    procedure SetDefault(const AValue: Boolean);
    function GetCancel: Boolean;
    procedure SetCancel(const AValue: Boolean);
    function GetKind: TBitBtnKind;
    procedure SetKind(const AValue: TBitBtnKind);
    function GetDropDownMenu: TPopupMenu;
    procedure SetDropDownMenu(const AValue: TPopupMenu);
    function GetStyle: TCustomButton.TButtonStyle;
    procedure SetStyle(const AValue: TCustomButton.TButtonStyle);
    function CanDropDownMenu: boolean;
    //Windows messages
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMMouseEnter(var Message: TNotifyEvent); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TNotifyEvent); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;

    function GetModalResult: TModalResult;
    procedure SetModalResult(const AValue: TModalResult);

    //Message Handlers for StyledButton
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMKeyDown(var Message: TMessage); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TMessage); message WM_KEYUP;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
    function GetFlat: Boolean;
    procedure SetFlat(const AValue: Boolean);

    //Glyph Support
    function HasCustomGlyph: Boolean;
    function GetGlyph: TBitmap;
    procedure SetGlyph(const AValue: TBitmap);
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(const AValue: TNumGlyphs);
    function GetMouseInControl: Boolean;
    function GetCursor: TCursor;
    function GetCaptionAlignment: TAlignment;
    procedure SetCaptionAlignment(const AValue: TAlignment);
    function GetCommandLinkHint: string;
    procedure SetCommandLinkHint(const AValue: string);
    function GetSpacing: Integer;
    procedure SetSpacing(const AValue: Integer);
    function GetLayout: TButtonLayout;
    procedure SetLayout(const AValue: TButtonLayout);
    function GetMargin: Integer;
    procedure SetMargin(const AValue: Integer);
    function IsCaptionAlignmentStored: Boolean;
    function GetElevationRequired: Boolean;
    procedure SetElevationRequired(const AValue: Boolean);
    function GetAsVCLComponent: Boolean;
    procedure SetAsVCLComponent(const AValue: Boolean);
    function GetActiveStyleName: string;
    function GetNotificationBadge: TNotificationBadgeAttributes;
    procedure SetNotificationBadge(const AValue: TNotificationBadgeAttributes);
    function GetShowCaption: Boolean;
    procedure SetShowCaption(const AValue: Boolean);
    function GetAllowAllUp: Boolean;
    function GetDown: Boolean;
    function GetGroupIndex: Integer;
    function IsCheckedStored: Boolean;
    procedure SetAllowAllUp(const AValue: Boolean);
    procedure SetDown(const AValue: Boolean);
    procedure SetGroupIndex(const AValue: Integer);
    function GetAutoClick: Boolean;
    function GetAutoClickDelay: Integer;
    procedure SetAutoClick(const AValue: Boolean);
    procedure SetAutoClickDelay(const AValue: Integer);
  protected
    procedure SetCursor(const AValue: TCursor); virtual;
    function CalcImageRect(var ATextRect: TRect;
      const AImageWidth, AImageHeight: Integer): TRect; virtual;
    function GetCaption: TCaption;
    function GetCaptionToDraw: TCaption; virtual;
    procedure SetCaption(const AValue: TCaption); virtual;
    function GetButtonState: TStyledButtonState; virtual;
    function GetImage(out AImageList: TCustomImageList;
      out AImageIndex: Integer): Boolean; virtual;
    function GetText: TCaption; virtual;
    function IsStoredStyleClass: Boolean; virtual;
    function IsEnabledStored: Boolean; virtual;
    function IsCaptionStored: Boolean; virtual;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure SetName(const AValue: TComponentName); override;
    procedure Loaded; override;
    {$IFDEF HiDPISupport}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ENDIF}
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    {$IFDEF D10_4+}
    procedure SetStyleName(const AValue: string); override;
    {$ENDIF}
    //for StyledButton
    procedure CreateWnd; override;
    function GetRenderClass: TStyledButtonRenderClass; virtual;
  public
    class procedure RegisterDefaultRenderingStyle(
      const ADrawType: TStyledButtonDrawType;
      const AFamily: TStyledButtonFamily = DEFAULT_CLASSIC_FAMILY;
      const AClass: TStyledButtonClass = DEFAULT_WINDOWS_CLASS;
      const AAppearance: TStyledButtonAppearance = DEFAULT_APPEARANCE;
      const AStyleRadius: Integer = DEFAULT_RADIUS;
      const ACursor: TCursor = DEFAULT_CURSOR); virtual;
    function GetRescalingButton: Boolean;
    procedure SetRescalingButton(const AValue: Boolean);
    function GetSplitButtonWidth: Integer;
    procedure ShowDropDownMenu;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SetButtonStyle(const AStyleFamily: TStyledButtonFamily;
      const AStyleClass: TStyledButtonClass;
      const AStyleAppearance: TStyledButtonAppearance); overload;
    procedure SetButtonStyle(const AStyleFamily: TStyledButtonFamily;
      const AModalResult: TModalResult); overload;
    procedure AssignStyleTo(ADestRender: TStyledButtonRender); overload;
    procedure AssignStyleTo(ADest: TCustomStyledButton); overload;
    procedure AssignTo(ADest: TPersistent); override;
    function AssignAttributes(
      const AEnabled: Boolean = True;
      const AImageList: TCustomImageList = nil;
      {$IFDEF D10_4+}const AImageName: string = '';{$ENDIF}
      const AImageIndex: Integer = -1;
      const AImageAlignment: TImageAlignment = iaLeft;
      const AAction: TCustomAction = nil;
      const AOnClick: TNotifyEvent = nil;
      const AName: string = ''): TCustomStyledButton;
    procedure DoDropDownMenu;
    constructor CreateStyled(AOwner: TComponent;
      const AFamily: TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance); overload; virtual;
    constructor CreateStyled(AOwner: TComponent;
      const AFamily: TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance;
      const ADrawType: TStyledButtonDrawType;
      const ACursor: TCursor;
      const AUseCustomDrawType: Boolean); overload; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ActiveStyleName: string read GetActiveStyleName;
    property AsVCLComponent: Boolean read GetAsVCLComponent write SetAsVCLComponent;
    property AutoClick: Boolean read GetAutoClick write SetAutoClick default False;
    property AutoClickDelay: Integer read GetAutoClickDelay write SetAutoClickDelay default DEFAULT_AUTOCLICK_DELAY;
    property ButtonState: TStyledButtonState read GetButtonState;
    property MouseInControl: Boolean read GetMouseInControl;
    property StyleApplied: Boolean read GetStyleApplied write SetStyleApplied;
    //For StyledButton
    procedure ReleasePaintBuffer;
    procedure Click; override;
    property Render: TStyledButtonRender read FRender;
    property RescalingButton: Boolean read GetRescalingButton write SetRescalingButton;
    property DoubleBuffered default True;
    property Enabled stored IsEnabledStored;
    property ParentFont default True;
    property Layout: TButtonLayout read GetLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read GetMargin write SetMargin default -1;
    property Spacing: Integer read GetSpacing write SetSpacing default 0;
    property StyleElements stored IsStoredStyleElements;
    property TabStop default True;
    property Caption: TCaption read GetText write SetText stored IsCaptionStored;
    property CaptionAlignment: TAlignment read GetCaptionAlignment write SetCaptionAlignment  Stored IsCaptionAlignmentStored;
    property ShowCaption: Boolean read GetShowCaption write SetShowCaption default True;
    property CommandLinkHint: string read GetCommandLinkHint write SetCommandLinkHint;
    property Cursor: TCursor read GetCursor write SetCursor default DEFAULT_CURSOR;
    property Default: Boolean read GetDefault write SetDefault default False;
    property Cancel: Boolean read GetCancel write SetCancel default False;
    property ImageAlignment: TImageAlignment read GetImageAlignment write SetImageAlignment default iaLeft;
    property DisabledImageIndex: TImageIndex read GetDisabledImageIndex write SetDisabledImageIndex default -1;
    property DisabledImages: TCustomImageList read GetDisabledImages write SetDisabledImages;
    property DropDownMenu: TPopupMenu read GetDropDownMenu write SetDropDownMenu;
    property ElevationRequired: Boolean read GetElevationRequired write SetElevationRequired default False;
    property Flat: Boolean read GetFlat write SetFlat default False;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored HasCustomGlyph;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property HotImageIndex: TImageIndex read GetHotImageIndex write SetHotImageIndex default -1;
    property StylusHotImageIndex: TImageIndex read GetStylusHotImageIndex write SetStylusHotImageIndex default -1;
    property Images: TCustomImageList read GetImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex stored IsImageIndexStored;
    property Kind: TBitBtnKind read GetKind write SetKind default bkCustom;
    property PressedImageIndex: TImageIndex read GetPressedImageIndex write SetPressedImageIndex default -1;
    property SelectedImageIndex: TImageIndex read GetSelectedImageIndex write SetSelectedImageIndex default -1;
    {$IFDEF D10_4+}
    property DisabledImageName: TImageName read GetDisabledImageName write SetDisabledImageName;
    property HotImageName: TImageName read GetHotImageName write SetHotImageName;
    property StylusHotImageName: TImageName read GetStylusHotImageName write SetStylusHotImageName;
    property ImageName: TImageName read GetImageName write SetImageName stored IsImageNameStored;
    property PressedImageName: TImageName read GetPressedImageName write SetPressedImageName;
    property SelectedImageName: TImageName read GetSelectedImageName write SetSelectedImageName;
    {$ENDIF}
    property ImageMargins: TImageMargins read GetImageMargins write SetImageMargins stored ImageMarginsStored;
    property ModalResult: TModalResult read GetModalResult write SetModalResult default mrNone;
    //Style as TButton
    property Style: TCustomButton.TButtonStyle read GetStyle write SetStyle default TCustomButton.TButtonStyle.bsPushButton;
    //Property used by TStyledButton, TStyledGraphicButton and TStyledSpeedButton
    property AllowAllUp: Boolean read GetAllowAllUp write SetAllowAllUp default False;
    property GroupIndex: Integer read GetGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read GetDown write SetDown stored IsCheckedStored default False;
    //StyledComponents Attributes
    property StyleRadius: Integer read GetStyleRadius write SetStyleRadius stored IsCustomRadius default DEFAULT_RADIUS;
    property StyleRoundedCorners: TRoundedCorners read GetStyleRoundedCorners write SetStyleRoundedCorners default ALL_ROUNDED_CORNERS;
    property StyleDrawType: TStyledButtonDrawType read GetStyleDrawType write SetStyleDrawType stored IsCustomDrawType;
    property StyleFamily: TStyledButtonFamily read GetStyleFamily write SetStyleFamily stored IsStoredStyleFamily;
    property StyleClass: TStyledButtonClass read GetStyleClass write SetStyleClass stored IsStoredStyleClass;
    property StyleAppearance: TStyledButtonAppearance read GetStyleAppearance write SetStyleAppearance stored IsStoredStyleAppearance;
    property Tag: Integer read GetTag write SetTag default 0;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default False;
    property ButtonStyleNormal: TStyledButtonAttributes read GetButtonStyleNormal write SetButtonStyleNormal stored IsStyleNormalStored;
    property ButtonStylePressed: TStyledButtonAttributes read GetButtonStylePressed write SetButtonStylePressed stored IsStylePressedStored;
    property ButtonStyleSelected: TStyledButtonAttributes read GetButtonStyleSelected write SetButtonStyleSelected stored IsStyleSelectedStored;
    property ButtonStyleHot: TStyledButtonAttributes read GetButtonStyleHot write SetButtonStyleHot stored IsStyleHotStored;
    property ButtonStyleDisabled: TStyledButtonAttributes read GetButtonStyleDisabled write SetButtonStyleDisabled stored IsStyleDisabledStored;
    property NotificationBadge: TNotificationBadgeAttributes read GetNotificationBadge write SetNotificationBadge stored IsNotificationBadgeStored;
    property OnDropDownClick: TNotifyEvent read GetOnDropDownClick write SetOnDropDownClick;

    //Property for StyledButton
    property TabOrder;
    property OnKeyDown;
    property OnKeyUp;
  end;

  { TStyledButton }
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TStyledButton = class(TCustomStyledButton)
  published
    property ActiveStyleName;
    property Action;
    property Align;
    property AllowAllUp;
    property GroupIndex;
    property Down;
    property Anchors;
    property AsVCLComponent stored False;
    property AutoClick;
    property AutoClickDelay;
    property BiDiMode;
    property Cancel;
    property Caption;
    property CaptionAlignment;
    property ShowCaption;
    property CommandLinkHint;
    property Constraints;
    property Cursor default DEFAULT_CURSOR;
    property Default;
    property DisabledImageIndex;
    {$IFDEF D10_4+}
    property DisabledImageName;
    {$ENDIF}
    property DisabledImages;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownMenu;
    property ElevationRequired;
    property Enabled;
    property Font;
    property HotImageIndex;
    {$IFDEF D10_4+}
    property HotImageName;
    {$ENDIF}
    property ImageAlignment;
    property ImageIndex;
    {$IFDEF D10_4+}
    property ImageName;
    {$ENDIF}
    property ImageMargins;
    property Images;
    property ModalResult;
    property NotificationBadge;
    property ParentBiDiMode;
    property ParentDoubleBuffered default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property PressedImageIndex;
    {$IFDEF D10_4+}
    property PressedImageName;
    {$ENDIF}
    property SelectedImageIndex;
    {$IFDEF D10_4+}
    property SelectedImageName;
    {$ENDIF}
    property ShowHint;
    property Spacing;
    property Style;
    property StylusHotImageIndex;
    property TabOrder;
    property TabStop;
    property Visible;
    property WordWrap;
    property StyleElements;
    {$IFDEF D10_4+}
    property StyleName;
    property StylusHotImageName;
    {$ENDIF}
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDownClick;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

    property Flat;
    property Glyph;
    property NumGlyphs;
    property Kind;
    property Tag;

    //StyledComponents Attributes
    property StyleRadius;
    property StyleRoundedCorners;
    property StyleDrawType;
    property StyleFamily;
    property StyleClass;
    property StyleAppearance;
    property ButtonStyleNormal;
    property ButtonStylePressed;
    property ButtonStyleSelected;
    property ButtonStyleHot;
    property ButtonStyleDisabled;
  end;

  { TStyledBitBtn }
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TStyledBitBtn = class(TCustomStyledButton)
  private
    FStyle: TButtonStyle;
  protected
    function IsCaptionStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Action;
    property Align;
    property Anchors;
    property AsVCLComponent stored False;
    property AutoClick;
    property AutoClickDelay;
    property BiDiMode;
    property Cancel;
    property Caption;
    property ShowCaption;
    property Constraints;
    property Cursor default DEFAULT_CURSOR;
    property Default;
    {$IFDEF D10_4+}
    property DisabledImageIndex;
    property DisabledImageName;
    {$ENDIF}
    property DoubleBuffered default True;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    {$IFDEF D10_4+}
    property HotImageIndex;
    property HotImageName;
    property ImageIndex;
    property ImageName;
    property Images;
    {$ENDIF}
    property Glyph;
    property Kind;
    property Layout;
    property Margin;
    property ModalResult;
    property NotificationBadge;
    property NumGlyphs;
    property Style: TButtonStyle read FStyle write FStyle;
    property Spacing default 4;
    property TabOrder;
    property TabStop;
    property Tag;
    property Visible;
    property WordWrap;
    property StyleElements;
    {$IFDEF D10_4+}
    property StyleName;
    {$ENDIF}
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

    //StyledComponents Attributes
    property StyleRadius;
    property StyleRoundedCorners;
    property StyleDrawType;
    property StyleFamily;
    property StyleClass;
    property StyleAppearance;
    property ButtonStyleNormal;
    property ButtonStylePressed;
    property ButtonStyleSelected;
    property ButtonStyleHot;
    property ButtonStyleDisabled;
  end;

//Global function to create a StyledButton
function CreateAndPosStyledButton(const AOwner: TComponent;
  const AParent: TWinControl;
  const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  const ACaption: TCaption;
  const ARectPosition: TRect): TCustomStyledButton;

implementation

uses
  System.Types
  , System.RTLConsts
  , System.StrUtils
  , System.DateUtils
  , Vcl.Forms
  {$IFDEF INCLUDE_BootstrapButtonStyles}
  , Vcl.BootstrapButtonStyles
  {$ENDIF}
  {$IFDEF INCLUDE_AngularButtonStyles}
  , Vcl.AngularButtonStyles
  {$ENDIF}
  {$IFDEF INCLUDE_ColorButtonStyles}
  , Vcl.ColorButtonStyles
  {$ENDIF}
  , Vcl.StyledCmpMessages
  ;

const
  DEFAULT_BTN_WIDTH = 75;
  DEFAULT_BTN_HEIGHT = 25;
  DEFAULT_IMAGE_HMARGIN = 8;
  DEFAULT_IMAGE_VMARGIN = 4;
  DefaultBitBtnGlyphSize = 18;
  BitBtnModalResults: array[TBitBtnKind] of TModalResult = (
    0, mrOk, mrCancel, 0, mrYes, mrNo, 0, mrAbort, mrRetry, mrIgnore,
    mrAll);

function CreateAndPosStyledButton(const AOwner: TComponent;
  const AParent: TWinControl;
  const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  const ACaption: TCaption;
  const ARectPosition: TRect): TCustomStyledButton;
begin
  Result := TCustomStyledButton.CreateStyled(AOwner, AFamily, AClass, AAppearance);
  Result.Parent := AParent;
  Result.Caption := ACaption;
  Result.SetBounds(ARectPosition.Left, ARectPosition.Top, ARectPosition.Right, ARectPosition.Bottom);
end;

{ TStyledButtonRender }

procedure TStyledButtonRender.AssignStyleTo(ADest: TStyledButtonRender);
begin
  ADest.AsVCLComponent := Self.AsVCLComponent;
  if not ParentFont then
    ADest.Font.Assign(Self.Font);
  ADest.FFlat := Self.FFlat;
  ADest.FStyle := Self.FStyle;
  ADest.FStyleRadius := Self.FStyleRadius;
  ADest.FStyleRoundedCorners := Self.FStyleRoundedCorners;
  ADest.FButtonStyleNormal.Assign(Self.FButtonStyleNormal);
  ADest.FButtonStylePressed.Assign(Self.FButtonStylePressed);
  ADest.FButtonStyleSelected.Assign(Self.FButtonStyleSelected);
  ADest.FButtonStyleHot.Assign(Self.FButtonStyleHot);
  ADest.FButtonStyleDisabled.Assign(Self.FButtonStyleDisabled);
  ADest.FNotificationBadge.Assign(Self.FNotificationBadge);
  ADest.SetButtonStyles(Self.FStyleFamily,
    Self.FStyleClass, Self.FStyleAppearance);
  ADest.FStyleDrawType := Self.FStyleDrawType;
  ADest.FCustomDrawType := Self.FCustomDrawType;
  ADest.FUseButtonLayout := Self.FUseButtonLayout;
  ADest.FButtonLayout := Self.FButtonLayout;
  ADest.Transparent := Self.FTransparent;
  ADest.FFlat := Self.FFlat;
  ADest.FCaptionAlignment := Self.FCaptionAlignment;
  ADest.FCommandLinkHint := Self.FCommandLinkHint;
  ADest.WordWrap := Self.WordWrap;

  if Assigned(FImages) then
  begin
    ADest.FImageMargins.Assign(FImageMargins);
    ADest.FImageAlignment := Self.FImageAlignment;
    ADest.Images := Images;
    ADest.ImageIndex := Self.ImageIndex;
    ADest.HotImageIndex := Self.HotImageIndex;
    ADest.StylusHotImageIndex := Self.StylusHotImageIndex;
    ADest.SelectedImageIndex := Self.SelectedImageIndex;
    ADest.PressedImageIndex := Self.PressedImageIndex;
    {$IFDEF D10_4+}
    ADest.ImageName := Self.ImageName;
    ADest.HotImageName := Self.HotImageName;
    ADest.StylusHotImageName := Self.StylusHotImageName;
    ADest.SelectedImageName := Self.SelectedImageName;
    ADest.PressedImageName := Self.PressedImageName;
    {$ENDIF}
  end;
  if Assigned(FDisabledImages) then
  begin
    ADest.DisabledImages := Self.DisabledImages;
    ADest.DisabledImageIndex := Self.DisabledImageIndex;
    {$IFDEF D10_4+}
    ADest.DisabledImageName := Self.DisabledImageName;
    {$ENDIF}
  end;
  if IsGlyphAssigned then
  begin
    ADest.FTransparentColor := FTransparentColor;
    ADest.NumGlyphs := FNumGlyphs;
    ADest.Glyph := FGlyph;
  end;
  ADest.FCustomDrawType := Self.FCustomDrawType;
end;

function TStyledButtonRender.AssignAttributes(
  const AEnabled: Boolean = True;
  const AImageList: TCustomImageList = nil;
  {$IFDEF D10_4+}const AImageName: string = '';{$ENDIF}
  const AImageIndex: Integer = -1;
  const AImageAlignment: TImageAlignment = iaLeft;
  const AAction: TCustomAction = nil;
  const AOnClick: TNotifyEvent = nil;
  const AName: string = ''): TControl;
begin
  Enabled := AEnabled;
  if Assigned(AImageList) then
  begin
    Images := AImageList;
    {$IFDEF D10_4+}
    if AImageName <> '' then
      ImageName := AImageName
    else
      ImageIndex := AImageIndex;
    {$ELSE}
    ImageIndex := AImageIndex;
    {$ENDIF}
    ImageAlignment := AImageAlignment;
    FUseButtonLayout := False;
  end;
  if Assigned(AAction) then
    Action := AAction
  else if Assigned(AOnClick) then
    FOnClick := AOnClick;
  if AName <> '' then
    FOwnerControl.Name := AName;
  Result := FOwnerControl;
end;

function TStyledButtonRender.AssignAttributes(
  const AEnabled: Boolean = True;
  const AImageList: TCustomImageList = nil;
  {$IFDEF D10_4+}const AImageName: string = '';{$ENDIF}
  const AImageIndex: Integer = -1;
  const AButtonLayout: TButtonLayout = blGlyphLeft;
  const AAction: TCustomAction = nil;
  const AOnClick: TNotifyEvent = nil;
  const AName: string = ''): TControl;
begin
  Enabled := AEnabled;
  if Assigned(AImageList) then
  begin
    Images := AImageList;
    {$IFDEF D10_4+}
    if AImageName <> '' then
      ImageName := AImageName
    else
      ImageIndex := AImageIndex;
    {$ELSE}
    ImageIndex := AImageIndex;
    {$ENDIF}
    Layout := AButtonLayout;
    FUseButtonLayout := True;
  end;
  if Assigned(AAction) then
    Action := AAction
  else if Assigned(AOnClick) then
    FOnClick := AOnClick;
  if AName <> '' then
    FOwnerControl.Name := AName;
  Result := FOwnerControl;
end;

procedure TStyledButtonRender.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TStyledButtonRender.EndUpdate;
begin
  Dec(FUpdateCount);
end;

function TStyledButtonRender.BitBtnCaptions(Kind: TBitBtnKind): string;
begin
  case Kind of
    bkOK: Result := STR_OK;
    bkCancel: Result := STR_CANCEL;
    bkHelp: Result := STR_HELP;
    bkYes: Result := STR_YES;
    bkNo: Result := STR_NO;
    bkClose: Result := STR_CLOSE;
    bkAbort: Result := STR_ABORT;
    bkRetry: Result := STR_RETRY;
    bkIgnore: Result := STR_IGNORE;
    bkAll: Result := STR_ALL;
  else
    Result := '';
  end;
end;

{$IFDEF HiDPISupport}
procedure TStyledButtonRender.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  if isDpiChange then
  begin
    FImageMargins.Left := MulDiv(FImageMargins.Left, M, D);
    FImageMargins.Top := MulDiv(FImageMargins.Top, M, D);
    FImageMargins.Right := MulDiv(FImageMargins.Right, M, D);
    FImageMargins.Bottom := MulDiv(FImageMargins.Bottom, M, D);
  end;
end;
{$ENDIF}

procedure TStyledButtonRender.CMEnabledChanged(var Message: TMessage);
begin
  if (not Enabled) then
    State := bsDisabled
  else
    State := bsUp;
  if FMouseInControl then
    FMouseInControl := False;
  Invalidate;
end;

procedure TStyledButtonRender.CMEnter(var Message: TCMEnter);
begin
  if not(Enabled) or (csDesigning in ComponentState) then
    Exit;
  FMouseInControl := False;
end;

procedure TStyledButtonRender.CMMouseEnter(var Message: TNotifyEvent);
begin
  if not(Enabled) or (csDesigning in ComponentState) then
    Exit;
  FMouseInControl := True;
  Invalidate;
end;

procedure TStyledButtonRender.CMMouseLeave(var Message: TNotifyEvent);
begin
  if not(Enabled) or (csDesigning in ComponentState) then
    Exit;
  State := bsUp;
  FMouseInControl := False;
  Invalidate;
end;

function TStyledButtonRender.UpdateCount: Integer;
begin
  Result := FUpdateCount;
end;

procedure TStyledButtonRender.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  ApplyButtonStyle;
end;

procedure TStyledButtonRender.Click(AKeyPressed: Boolean);
var
  Form: TCustomForm;
begin
  if not AKeyPressed and FMouseOverDropDown then
  begin
    State := bsUp;
    Invalidate;
    DoDropDownMenu;
  end
  else
  begin
    Form := GetParentForm(FOwnerControl);
    if Form <> nil then
      Form.ModalResult := ModalResult;
    FMouseInControl := True;
    OwnerControl.Repaint;
    //Reset AutoClick for Button (if enabled)
    AutoClick := False;
    OnClick(FOwnerControl);
  end;
end;

procedure TStyledButtonRender.ImageListChange(Sender: TObject);
begin
  UpdateImageIndexAndName;
  Invalidate;
end;

procedure TStyledButtonRender.ImageMarginsChange(Sender: TObject);
begin
  Invalidate;
end;

constructor TStyledButtonRender.CreateStyled(AOwner: TControl;
  const AOnClick: TNotifyEvent;
  const AControlFont: TControlFont;
  const AGetCaption: TGetCaption;
  const ASetCaption: TSetCaption;
  const AGetParentFont: TGetParentFont;
  const ASetParentFont: TSetParentFont;
  const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  const ADrawType: TStyledButtonDrawType;
  const ACursor: TCursor;
  const AUseCustomDrawType: Boolean);
begin
  Assert(Assigned(AOwner));
  inherited Create;
  FTransparentColor := clOlive;
  FTransparent := False;
  FImageIndex := -1;
  FNumGlyphs := 1;
  FCaptionAlignment := TAlignment.taCenter;
  FFlat := False;
  FShowCaption := True;
  //Owner Control "link"
  FOwnerControl := AOwner;
  if (FOwnerControl is TCustomStyledButton) then
    TCustomStyledButton(FOwnerControl).FRender := Self
  else if (FOwnerControl is TCustomStyledGraphicButton) then
    TCustomStyledGraphicButton(FOwnerControl).FRender := Self
  else
    raise EStyledButtonError.Create(ERROR_CANNOT_USE_RENDER);

  FOnClick := AOnClick;
  FControlFont := AControlFont;
  FGetCaption := AGetCaption;
  FSetCaption := ASetCaption;
  FGetParentFont := AGetParentFont;
  FSetParentFont := ASetParentFont;

  FDisabledImageIndex := -1;
  FHotImageIndex := -1;
  FStylusHotImageIndex := -1;
  FImageAlignment := iaLeft;
  ImageIndex := -1;
  FPressedImageIndex := -1;
  FSelectedImageIndex := -1;
  FSpacing := 0;
  FMargin := -1;

  FStyle := TCustomButton.TButtonStyle.bsPushButton;
  FButtonStyleNormal := TStyledButtonAttributes.Create(AOwner);
  FButtonStyleNormal.Name := 'Normal';
  FButtonStylePressed := TStyledButtonAttributes.Create(AOwner);
  FButtonStylePressed.Name := 'Pressed';
  FButtonStyleSelected := TStyledButtonAttributes.Create(AOwner);
  FButtonStyleSelected.Name := 'Selected';
  FButtonStyleHot := TStyledButtonAttributes.Create(AOwner);
  FButtonStyleHot.Name := 'Hot';
  FButtonStyleDisabled := TStyledButtonAttributes.Create(AOwner);
  FButtonStyleDisabled.Name := 'Disabled';
  FNotificationBadge := TNotificationBadgeAttributes.Create(AOwner);
  FNotificationBadge.Name := 'NotificationBadge';
  FOwnerControl.ControlStyle := [csOpaque, csCaptureMouse, csClickEvents,
    csSetCaption, csDoubleClicks];
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FImageMargins := TImageMargins.Create;
  FImageMargins.Left := DEFAULT_IMAGE_HMARGIN;
  FImageMargins.OnChange := ImageMarginsChange;
  FImageAlignment := iaLeft;
  FCustomDrawType := False;
  FOwnerControl.Cursor := ACursor;
  ParentFont := True;
  FOwnerControl.Width := DEFAULT_BTN_WIDTH;
  FOwnerControl.Height := DEFAULT_BTN_HEIGHT;
  FMouseInControl := False;

  //AutoClick initialization
  FAutoClick := False;
  FAutoClickDelay := DEFAULT_AUTOCLICK_DELAY;
  FAutoClickTimer := nil;

  //Style initialization
  FStyleDrawType := ADrawType;
  FCustomDrawType := AUseCustomDrawType;
  FStyleRadius := DEFAULT_RADIUS;
  FStyleRoundedCorners := ALL_ROUNDED_CORNERS;
  FStyleFamily := AFamily;
  FStyleAppearance := AAppearance;
  //Call the Setter of StyleClass!
  StyleClass := AClass;
end;

constructor TStyledButtonRender.Create(AOwner: TControl;
  const AOnClick: TNotifyEvent;
  const AControlFont: TControlFont;
  const AGetCaption: TGetCaption;
  const ASetCaption: TSetCaption;
  const AGetParentFont: TGetParentFont;
  const ASetParentFont: TSetParentFont);
begin
  CreateStyled(AOwner,
    AOnClick,
    AControlFont,
    AGetCaption,
    ASetCaption,
    AGetParentFont,
    ASetParentFont,
    DEFAULT_CLASSIC_FAMILY,
    DEFAULT_WINDOWS_CLASS,
    DEFAULT_APPEARANCE,
    DEFAULT_STYLEDRAWTYPE,
    DEFAULT_CURSOR,
    False);
end;

procedure TStyledButtonRender.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then
  begin
    if not CheckDefaults or (ImageIndex = -1) then
      ImageIndex := TCustomAction(Sender).ImageIndex;
  {$IFDEF D10_4+}
    if not CheckDefaults or (ImageName = '') then
      ImageName := TCustomAction(Sender).ImageName;
  {$ENDIF}
  end;
end;

function TStyledButtonRender.ApplyButtonStyle: Boolean;
var
  LButtonFamily: TButtonFamily;
  LStyleClass: TStyledButtonClass;
  LStyleAppearance: TStyledButtonAppearance;
begin
  if AsVCLStyle then
  begin
    //if StyleElements contains seClient then use
    //VCL Style assigned to Button or Global VCL Style
    if seBorder in FOwnerControl.StyleElements then
      LStyleAppearance := DEFAULT_APPEARANCE;
    LStyleClass := GetActiveStyleName;
  end
  else
  begin
    LStyleClass := FStyleClass;
    LStyleAppearance := FStyleAppearance;
  end;
  Result := StyleFamilyCheckAttributes(FStyleFamily,
    LStyleClass, LStyleAppearance, LButtonFamily);
  if Result (*or (csDesigning in ComponentState)*) then
  begin
    StyleFamilyUpdateAttributes(
      FStyleFamily,
      LStyleClass,
      LStyleAppearance,
      FButtonStyleNormal,
      FButtonStylePressed,
      FButtonStyleSelected,
      FButtonStyleHot,
      FButtonStyleDisabled);

    if not FCustomDrawType then
      FStyleDrawType := FButtonStyleNormal.DrawType;
  end
  else
  begin
    FStyleClass := LStyleClass;
    FStyleAppearance := LStyleAppearance;
  end;
  if Result then
    Invalidate;
end;

function TStyledButtonRender.IsCaptionAlignmentStored: Boolean;
begin
  if (Style = TCustomButton.TButtonStyle.bsCommandLink) then
    Result := CaptionAlignment <> taLeftJustify
  else
    Result := CaptionAlignment <> taCenter;
end;

destructor TStyledButtonRender.Destroy;
begin
  AutoClick := False;
  Images := nil;
  FreeAndNil(FAutoClickTimer);
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FImageMargins);
  FreeAndNil(FButtonStyleNormal);
  FreeAndNil(FButtonStylePressed);
  FreeAndNil(FButtonStyleSelected);
  FreeAndNil(FButtonStyleHot);
  FreeAndNil(FButtonStyleDisabled);
  FreeAndNil(FNotificationBadge);
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

function TStyledButtonRender.CanDropDownMenu: boolean;
begin
  Result := (FStyle = TCustomButton.TButtonStyle.bsSplitButton) and
    (Assigned(DropDownMenu) or Assigned(FOnDropDownClick));
end;

procedure TStyledButtonRender.ShowDropDownMenu;
var
  Pt: TPoint;
  LRect: TRect;
begin
  if Assigned(FDropDownMenu) then
  begin
    LRect := FOwnerControl.ClientRect;
    Pt := FOwnerControl.ClientToScreen(Point(LRect.Left, LRect.Bottom));
    FDropDownMenu.PopupComponent := FOwnerControl;
    FDropDownMenu.Popup(Pt.X, Pt.Y);
  end;
end;

procedure TStyledButtonRender.DoDropDownMenu;
begin
  if Assigned(FOnDropDownClick) then
    FOnDropDownClick(Self)
  else if Assigned(FDropDownMenu) then
    ShowDropDownMenu;
end;

procedure TStyledButtonRender.WMKeyDown(var Message: TMessage);
begin
  if Message.WParam = VK_SPACE then
  begin
    State := bsDown;
    FMouseInControl := True;
    Invalidate;
  end;
end;

function TStyledButtonRender.GetHandle: HWND;
begin
  if (FOwnerControl is TCustomStyledButton) then
  begin
    if TCustomStyledButton(FOwnerControl).HandleAllocated then
      Result := TCustomStyledButton(FOwnerControl).Handle
    else
      Result := 0;
  end
  else
    Result := 0;
end;

procedure TStyledButtonRender.WMKeyUp(var Message: TMessage);
var
  P: TPoint;
begin
  if Message.WParam = VK_SPACE then
  begin
    State := bsUp;
    P := Mouse.CursorPos;
    FMouseInControl := WindowFromPoint(P) = GetHandle;
    Invalidate;
    Click(True);
  end;
end;

function TStyledButtonRender.GetText: TCaption;
begin
  Assert(Assigned(FGetCaption));
  Result := FGetCaption;
end;

function TStyledButtonRender.HasTransparentParts: Boolean;
begin
  Result := (FStyleDrawType <> btRect) or FTransparent;
end;

function TStyledButtonRender.GetImage(out AImageList: TCustomImageList;
  out AImageIndex: Integer): Boolean;
begin
  if (FOwnerControl is TCustomStyledButton) then
    Result := TCustomStyledButton(FOwnerControl).GetImage(AImageList, AImageIndex)
  else if (FOwnerControl is TCustomStyledGraphicButton) then
    Result := TCustomStyledGraphicButton(FOwnerControl).GetImage(AImageList, AImageIndex)
  else
  begin
    AImageList := nil;
    AImageIndex := -1;
    Result := False;
  end;
end;

function TStyledButtonRender.GetInternalImage(out AImageList: TCustomImageList;
  out AImageIndex: Integer): Boolean;
begin
  case ButtonState of
    bsmNormal:
    begin
      AImageList := FImages;
      AImageIndex := ImageIndex;
    end;
    bsmPressed:
    begin
      AImageList := FImages;
      if FPressedImageIndex <> -1 then
        AImageIndex := FPressedImageIndex
      else
        AImageIndex := ImageIndex;
    end;
    bsmHot:
    begin
      AImageList := FImages;
      if (FHotImageIndex <> -1) then
        AImageIndex := FHotImageIndex
      else if (FStylusHotImageIndex <> -1) then
        AImageIndex := FStylusHotImageIndex
      else
        AImageIndex := ImageIndex;
    end;
    bsmSelected:
    begin
      AImageList := FImages;
      if FSelectedImageIndex <> -1 then
        AImageIndex := FSelectedImageIndex
      else
        AImageIndex := ImageIndex;
    end;
    bsmDisabled:
    begin
      if Assigned(FDisabledImages) then
        AImageList := FDisabledImages
      else
        AImageList := FImages;
      if FDisabledImageIndex <> -1 then
        AImageIndex := FDisabledImageIndex
      else
        AImageIndex := ImageIndex;
    end;
  end;
  Result := Assigned(AImageList) and (AImageIndex <> -1);
end;

function TStyledButtonRender.GetKind: TBitBtnKind;
begin
  if FKind <> bkCustom then
    if ((FKind in [bkOK, bkYes]) xor Default) or
      ((FKind in [bkCancel, bkNo]) xor Cancel) or
      (ModalResult <> BitBtnModalResults[FKind]) then
      FKind := bkCustom;
  Result := FKind;
end;

function TStyledButtonRender.GetMouseInControl: Boolean;
begin
  Result := FMouseInControl;
end;

function TStyledButtonRender.IsCustomDrawType: Boolean;
begin
  Result := FCustomDrawType;
end;

function TStyledButtonRender.IsDefaultAppearance: Boolean;
begin
  Result := (FStyleFamily = DEFAULT_APPEARANCE) and
    (FStyleClass = DEFAULT_WINDOWS_CLASS) and
    (FStyleAppearance = DEFAULT_APPEARANCE);
end;

function TStyledButtonRender.IsPressed: Boolean;
begin
  Result := FState = bsDown;
end;

function TStyledButtonRender.IsCustomRadius: Boolean;
begin
  Result := StyleRadius <> DEFAULT_RADIUS;
end;

function TStyledButtonRender.IsStoredStyleFamily: Boolean;
begin
  Result := FStyleFamily <> DEFAULT_CLASSIC_FAMILY;
end;

function TStyledButtonRender.AsVCLStyle: Boolean;
begin
  Result := (FStyleFamily = DEFAULT_CLASSIC_FAMILY) and
    (seClient in FOwnerControl.StyleElements);
end;

function TStyledButtonRender.GetAsVCLComponent: Boolean;
begin
  Result := (FStyleFamily = DEFAULT_CLASSIC_FAMILY) and
    (seClient in FOwnerControl.StyleElements);
end;

procedure TStyledButtonRender.SetAsVCLComponent(const AValue: Boolean);
begin
  if AValue <> GetAsVCLComponent then
  begin
    if AValue then
    begin
      FStyleFamily := DEFAULT_CLASSIC_FAMILY;
      FStyleClass := DEFAULT_WINDOWS_CLASS;
      FStyleAppearance := DEFAULT_APPEARANCE;
      FOwnerControl.StyleElements := FOwnerControl.StyleElements + [seClient];
    end
    else if FStyleFamily = DEFAULT_CLASSIC_FAMILY then
    begin
      FOwnerControl.StyleElements := FOwnerControl.StyleElements - [seClient];
    end;
    ApplyButtonStyle;
  end;
end;

procedure TStyledButtonRender.AutoClickOnTimer(Sender: TObject);
var
  FInterval: TDateTime;
  LOffSetWidth: Integer;
begin
  if not (csDesigning in FOwnerControl.ComponentState) then
  begin
    //If the AutoClickTimer is less then FAutoClickDelay
    //then Paint a portion of Button of another hot color
    FInterval := System.DateUtils.MilliSecondsBetween(Now, FStartAutoClick);
    if FInterval < FAutoClickDelay then
    begin
      //Time for AutoClick in progress: repaint Button
      if (FStyleDrawType = btRounded) then
        LOffSetWidth := FOwnerControl.Height
      else if (FStyleDrawType = btRoundRect) then
        LOffSetWidth := FStyleRadius
      else
        LOffSetWidth := 0;

      FAutoClickPixels := LOffSetWidth +
        Round((FOwnerControl.Width - LOffSetWidth) / (FAutoClickDelay / FInterval));
      Invalidate;
    end
    else
    begin
      //Auto click execution
      if (FOwnerControl.Enabled) and (State <> bsDown) then
      begin
        //Disable AutoClick before click
        AutoClick := False;
        SetFocus;
        Invalidate;
        OnClick(FOwnerControl);
      end;
    end;
  end;
end;

procedure TStyledButtonRender.UpdateAutoClickTimer(const AReset: Boolean);
begin
  if not Assigned(FAutoClickTimer) then
  begin
    FAutoClickTimer := TTimer.Create(nil);
    FAutoClickTimer.OnTimer := AutoClickOnTimer;
  end;
  //Reset Start Time
  if AReset then
  begin
    FStartAutoClick := Now;
    FAutoClickPixels := 0;
  end;
  //Calculate Interval based on width of Control for pixel painting
  FAutoClickTimer.Interval := FAutoClickDelay div FOwnerControl.Width;
  //Enable Timer
  FAutoClickTimer.Enabled := FAutoClick;
end;

procedure TStyledButtonRender.SetAutoClick(const AValue: Boolean);
begin
  if FAutoClick <> AValue then
  begin
    FAutoClick := AValue;
    UpdateAutoClickTimer(AValue);
  end;
end;

procedure TStyledButtonRender.SetAutoClickDelay(const AValue: Integer);
begin
  if FAutoClickDelay <> AValue then
  begin
    FAutoClickDelay := AValue;
    UpdateAutoClickTimer(AValue <> 0);
  end;
end;

function TStyledButtonRender.IsStoredStyleClass: Boolean;
var
  LClass: TStyledButtonClass;
  LAppearance: TStyledButtonAppearance;
  LButtonFamily: TButtonFamily;
begin
  StyleFamilyCheckAttributes(FStyleFamily, LClass, LAppearance, LButtonFamily);
  if FModalResult <> mrNone then
    LButtonFamily.StyledAttributes.GetStyleByModalResult(FModalResult, LClass, LAppearance);
  Result := FStyleClass <> LClass;
end;

function TStyledButtonRender.IsStoredStyleAppearance: Boolean;
var
  LClass: TStyledButtonClass;
  LAppearance: TStyledButtonAppearance;
  LButtonFamily: TButtonFamily;
  LModalResultAppearance: TStyledButtonAppearance;
begin
  StyleFamilyCheckAttributes(FStyleFamily, LClass, LAppearance, LButtonFamily);
  if FModalResult <> mrNone then
  begin
    LButtonFamily.StyledAttributes.GetStyleByModalResult(FModalResult,
      LClass, LModalResultAppearance);
    Result := FStyleAppearance <> LModalResultAppearance;
  end
  else
    Result := FStyleAppearance <> LAppearance;
end;

function TStyledButtonRender.IsStoredStyleElements: Boolean;
begin
  if FStyleFamily = DEFAULT_CLASSIC_FAMILY then
    Result := FOwnerControl.StyleElements <> [seFont, seClient, seBorder]
  else
    Result := False;
end;

function TStyledButtonRender.IsNotificationBadgeStored: Boolean;
begin
  Result := FNotificationBadge.HasCustomAttributes;
end;

function TStyledButtonRender.IsStyleDisabledStored: Boolean;
begin
  Result := FButtonStyleDisabled.HasCustomAttributes;
end;

function TStyledButtonRender.IsStylePressedStored: Boolean;
begin
  Result := FButtonStylePressed.HasCustomAttributes;
end;

function TStyledButtonRender.IsStyleSelectedStored: Boolean;
begin
  Result := FButtonStyleSelected.HasCustomAttributes;
end;

function TStyledButtonRender.IsStyleHotStored: Boolean;
begin
  Result := FButtonStyleHot.HasCustomAttributes;
end;

function TStyledButtonRender.IsStyleNormalStored: Boolean;
begin
  Result := FButtonStyleNormal.HasCustomAttributes;
end;

function TStyledButtonRender.GetAttributes(const AMode: TStyledButtonState): TStyledButtonAttributes;
begin
  case Amode of
    bsmPressed: Result := FButtonStylePressed;
    bsmSelected: Result := FButtonStyleSelected;
    bsmHot: Result := FButtonStyleHot;
    bsmDisabled: Result := FButtonStyleDisabled;
  else
    Result := FButtonStyleNormal;
  end;
end;

procedure TStyledButtonRender.DrawNotificationBadge(
  const ACanvas: TCanvas; const ASurfaceRect: TRect);
var
  LScaleFactor: Single;
begin
  if not FNotificationBadge.IsVisible then
    Exit;

  LScaleFactor := GetOwnerScaleFactor;
  DrawButtonNotificationBadge(ACanvas, ASurfaceRect, LScaleFactor,
    FNotificationBadge.BadgeContent,
    FNotificationBadge.Size, FNotificationBadge.Position,
    FNotificationBadge.Color,
    FNotificationBadge.FontColor, FNotificationBadge.FontStyle);
end;

procedure TStyledButtonRender.DrawBackgroundAndBorder(
  const ACanvas: TCanvas;
  const AEraseBackground: Boolean);
var
  LDrawRect: TRect;
  LButtonOffset: Integer;
  LStyleAttribute, LDrawAttribute: TStyledButtonAttributes;
  LCorners: TRoundedCorners;
  LDrawingAutoClick: Boolean;
begin
  LStyleAttribute := GetDrawingStyle(ACanvas, ButtonState);

  //Erase Background
  if AEraseBackground then
    EraseBackground(ACanvas.Handle);

  //Don't draw button border for Flat Buttons
  if FFlat and not FMouseInControl and not Focused then
    ACanvas.Pen.Style := psClear;

  //Don't draw button face for Transparent Buttons
  if FTransparent and not FDown and not FMouseInControl and not Focused then
    ACanvas.Brush.Style := bsClear;

  //Draw Button Shape
  LDrawRect := FOwnerControl.ClientRect;
  CanvasDrawshape(ACanvas, LDrawRect, FStyleDrawType,
    FStyleRadius*GetOwnerScaleFactor, FStyleRoundedCorners);

  //Draw Progress Background in AutoClick mode
  LDrawingAutoClick := (FAutoClick) and (FAutoClickPixels > 0);
  if LDrawingAutoClick then
  begin
    LDrawRect := FOwnerControl.ClientRect;
    LDrawRect.Width := FAutoClickPixels;
    LCorners := FStyleRoundedCorners;
    //Change Canvas State
    //Use Different Color to Draw the progress BackGround, different from "hot" state
    if FMouseInControl or Focused then
      LDrawAttribute := GetDrawingStyle(ACanvas, bsmNormal)
    else
      LDrawAttribute := GetDrawingStyle(ACanvas, bsmHot);
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := LDrawAttribute.ButtonColor;
    //Draw progress Background
    CanvasDrawshape(ACanvas, LDrawRect, FStyleDrawType,
      FStyleRadius*GetOwnerScaleFactor, LCorners);
    //Restore Canvas State
    LStyleAttribute := GetDrawingStyle(ACanvas, ButtonState);
  end;

  //Draw Bar and Triangle
  if FDropDownRect.Width > 0 then
  begin
    if FFlat then
      ACanvas.Pen.Style := psClear;
    if not (StyleDrawType in [btRounded, btEllipse]) then
    begin
      CanvasDrawBar(ACanvas, FDropDownRect,
        GetOwnerScaleFactor,
        ACanvas.Pen.Color);
      CanvasDrawTriangle(ACanvas, FDropDownRect,
        GetOwnerScaleFactor,
        LStyleAttribute.FontColor);
    end
    else
    begin
      LButtonOffset := FDropDownRect.Height div 8;
      FDropDownRect.Left := FDropDownRect.Left - LButtonOffset;
      FDropDownRect.Right := FDropDownRect.Right - LButtonOffset;
      CanvasDrawTriangle(ACanvas, FDropDownRect,
        GetOwnerScaleFactor,
        LStyleAttribute.FontColor);
    end;
  end;
end;

function TStyledButtonRender.CalcMaxBorderWidth: Integer;
begin
  Result := Max(Max(Max(Max(FButtonStyleNormal.BorderWidth,
    FButtonStylePressed.BorderWidth),
    FButtonStyleSelected.BorderWidth),
    FButtonStyleHot.BorderWidth),
    FButtonStyleDisabled.BorderWidth);
end;

function TStyledButtonRender.CalcImageRect(const ASurfaceRect: TRect;
  const AImageWidth, AImageHeight: Integer): TRect;
var
  LTextRect: TRect;
begin
  CalcImageAndTextRect(ASurfaceRect, Caption, LTextRect, Result,
    AImageWidth, AImageHeight, FImageAlignment, FImageMargins,
    CalcMaxBorderWidth, GetOwnerScaleFactor);
end;

procedure TStyledButtonRender.SetButtonStyles(
  const AFamily: TStyledButtonFamily; const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance);
begin
  if (AFamily <> FStyleFamily) or
    (AClass <> FStyleClass) or
    (AAppearance <> FStyleAppearance) then
  begin
    FStyleFamily := AFamily;
    FStyleClass := AClass;
    FStyleAppearance := AAppearance;
    ApplyButtonStyle;
  end;
end;

{$IFDEF D10_4+}
procedure TStyledButtonRender.CheckImageIndexes;
var
  LImageIndex: TImageIndex;
  LImageName: TImageName;
begin
  if (Images = nil) or not Images.IsImageNameAvailable then
    Exit;
  LImageIndex := ImageIndex;
  LImageName := ImageName;
  Images.CheckIndexAndName(LImageIndex, LImageName);
  if LImageIndex <> ImageIndex then
    ImageIndex := LImageIndex;
  if LImageName <> ImageName then
    ImageName := LImageName;
  Images.CheckIndexAndName(FHotImageIndex, FHotImageName);
  Images.CheckIndexAndName(FPressedImageIndex, FPressedImageName);
  Images.CheckIndexAndName(FSelectedImageIndex, FSelectedImageName);
  Images.CheckIndexAndName(FStylusHotImageIndex, FStylusHotImageName);
  if FDisabledImages <> nil then
    FDisabledImages.CheckIndexAndName(FDisabledImageIndex, FDisabledImageName)
  else
    Images.CheckIndexAndName(FDisabledImageIndex, FDisabledImageName);
end;
{$ENDIF}

procedure TStyledButtonRender.EraseBackground(const ACanvasHandle: HDC);
var
  LStyle: TCustomStyleServices;
  LHandle: HWND;
begin
  LStyle := StyleServices;
  if HasTransparentParts then
  begin
    if FOwnerControl is TWinControl then
      LHandle := TWinControl(FOwnerControl).Handle
    else
      LHandle := 0;
    if OwnerControl is TCustomStyledGraphicButton then
    begin
      if LStyle.Available and Transparent then
        LStyle.DrawParentBackground(LHandle, ACanvasHandle, nil, False)
      else
        PerformEraseBackground(FOwnerControl, ACanvasHandle);
    end
    else
    begin
      if LStyle.Available then
        LStyle.DrawParentBackground(LHandle, ACanvasHandle, nil, False)
      else
        PerformEraseBackground(FOwnerControl, ACanvasHandle);
    end;
  end;
end;

function TStyledButtonRender.GetImageSize(out AWidth, AHeight: Integer;
  out AImageList: TCustomImageList; out AImageIndex: Integer): boolean;
begin
  AWidth := 0;
  AHeight := 0;
  //Return True if using ImageList
  Result := GetImage(AImageList, AImageIndex);
  if Result then
  begin
    AWidth := AImageList.Width;
    AHeight := AImageList.Height;
  end
  else if ((FKind = bkCustom) and IsGlyphAssigned) or (FKind <> bkCustom) then
  begin
    if (FKind = bkCustom) and (FNumGlyphs > 0) then
    begin
      AWidth := FGlyph.Width div FNumGlyphs;
      AHeight := FGlyph.Height;
    end
    else
    begin
      {$IFDEF D10_4+}
      AWidth := FOwnerControl.ScaleValue(DefaultBitBtnGlyphSize);
      {$ELSE}
      AWidth := DefaultBitBtnGlyphSize;
      {$ENDIF}
      AHeight := AWidth;
    end;
  end;
end;

procedure TStyledButtonRender.DrawCaptionAndImage(const ACanvas: TCanvas;
  const ASurfaceRect: TRect);
var
  LTextFlags: Cardinal;
  LImageRect, LTextRect: TRect;
  LImageList: TCustomImageList;
  LImageIndex: Integer;
  LImageWidth, LImageHeight: Integer;
  LUseImageList: Boolean;
  LGlyphPos: TPoint;
  LCaption: TCaption;
begin
  if FShowCaption then
    LCaption := GetCaptionToDraw
  else
    LCaption := '';
  case FCaptionAlignment of
    taLeftJustify: LTextFlags := DT_LEFT;
    taRightJustify: LTextFlags := DT_RIGHT;
  else
    LTextFlags := DT_CENTER;
  end;
  if FWordWrap then
    LTextFlags := LTextFlags or DT_WORDBREAK
  else
    LTextFlags := LTextFlags or DT_VCENTER;
  LTextFlags := FOwnerControl.DrawTextBiDiModeFlags(LTextFlags);
  LUseImageList := GetImageSize(LImageWidth, LImageHeight,
    LImageList, LImageIndex);

  //FUseButtonLayout is used by TStyledBitBtn and TStyledSpeedButton
  //to use Layout for Icon position
  if FUseButtonLayout then
  begin
    //Calculate LTextRect and LImageRect using Margin, Spacing and ButtonLayout
    CalcImageAndTextRect(ACanvas, LCaption, ASurfaceRect,
      TPoint.Create(0,0), LGlyphPos, LTextRect,
      LImageWidth, LImageHeight, FButtonLayout, FMargin, FSpacing, LTextFlags);
    LImageRect.Left := LGlyphPos.X;
    LImageRect.Top := LGlyphPos.Y;
    LImageRect.Width := LImageWidth;
    LImageRect.Height := LImageHeight;
  end
  else
  begin
    if Style = bsCommandLink then
    begin
      CalcImageAndTextRect(ASurfaceRect, LCaption, LTextRect, LImageRect,
        LImageWidth, LImageHeight, iaLeft, FImageMargins,
        0, GetOwnerScaleFactor);
      if Assigned(Images) then
      begin
        //A CommandLink Buttons Ignores ImageAlignment and ImagePosition
        //Fixed Left and Right of ImageRect
        LImageRect.Right := LImageRect.Right - Round(8*GetOwnerScaleFactor);
        LImageRect.Left := Round(8*GetOwnerScaleFactor);
        LTextRect.Left := LImageWidth + FImageMargins.Left + FImageMargins.Right +
          Round(8*GetOwnerScaleFactor);
      end
      else
      begin
        //Fixed Size of ImageRect
        LImageRect.top := Round(16*GetOwnerScaleFactor);
        LImageRect.Left := Round(10*GetOwnerScaleFactor);
        LImageRect.Height := Round(20*GetOwnerScaleFactor);
        LImageRect.Width := LImageRect.Height;
        if AsVCLComponent then
          DrawIconFromCommandLinkRes(ACanvas, LImageRect,
            Self.ActiveStyleName, FState, Enabled)
        else
          DrawIconFromCommandLinkRes(ACanvas, LImageRect,
            Self.FStyleClass, FState, Enabled);
      end;
    end
    else
    begin
      //Calculate LTextRect and LImageRect using ImageMargins and ImageAlignment
      CalcImageAndTextRect(ASurfaceRect, LCaption, LTextRect, LImageRect,
        LImageWidth, LImageHeight, FImageAlignment, FImageMargins,
        CalcMaxBorderWidth, GetOwnerScaleFactor);
    end;
  end;
  if ElevationRequired then
  begin
    //Load the Shield Icon from Resource
    DrawIconFromCommandLinkRes(ACanvas, LImageRect,
      RESOURCE_SHIELD_ICON, FState, Enabled)
  end
  else if LUseImageList then
  begin
    //Uses an ImageList to draw the Icon
    LImageList.Draw(ACanvas, LImageRect.Left, LImageRect.Top,
      LImageIndex, Enabled);
  end
  else
  begin
    if ((FKind = bkCustom) and IsGlyphAssigned) or (FKind <> bkCustom) then
    begin
      //Uses the Glyph to draw the Icon
      DrawBitBtnGlyph(ACanvas, LImageRect, FKind, FState, Enabled,
        FGlyph, FNumGlyphs, FTransparentColor);
    end;
  end;
  if (Style = bsCommandLink) then
  begin
    //Load the Arrow Icon from Resource
    if Assigned(Images) then
      LTextRect.Left := LImageRect.Right+Round(10*GetOwnerScaleFactor)
    else
      LTextRect.Left := Round(38*GetOwnerScaleFactor);
    LTextRect.Right := ASurfaceRect.Right;
    ACanvas.Font.Height := Round(-16*GetOwnerScaleFactor);
    if AsVCLComponent and (ActiveStyleName = 'Windows') then
      ACanvas.Font.Color := HtmlToColor('#0279D7'); //Windows Blue color
    //Calculate TextRect: WordWrap, centered
    LTextFlags := DT_VCENTER or DT_WORDBREAK;
    Winapi.Windows.DrawText(ACanvas.Handle, PChar(LCaption),
      Length(LCaption), LTextRect, LTextFlags or DT_CALCRECT);
    //WordWrap but not vertical centerer: fixed top
    LTextFlags := DT_WORDBREAK;
    LTextRect.Top := Round(28*GetOwnerScaleFactor);
    DrawButtonText(ACanvas, LCaption, taLeftJustify, FSpacing, CalcMaxBorderWidth,
      LTextRect, LTextFlags, False);
    if FCommandLinkHint <> '' then
    begin
      ACanvas.Font.Height := Round(-11*GetOwnerScaleFactor);
      //Draw Command Link Hint Under Caption
      LTextRect.Top := LTextRect.Top + LTextRect.Height;
      LTextRect.Bottom := ASurfaceRect.Bottom;
      LTextRect.Right := ASurfaceRect.Right - Round(4*GetOwnerScaleFactor);
      //Calculate TextRect: WordWrap, centered
      LTextFlags := DT_VCENTER or DT_WORDBREAK;
      Winapi.Windows.DrawText(ACanvas.Handle, PChar(FCommandLinkHint),
        Length(FCommandLinkHint), LTextRect, LTextFlags or DT_CALCRECT);
      //WordWrap but not vertical centerer: fixed top
      LTextFlags := DT_WORDBREAK;
      OffsetRect(LTextRect, 0, Round(15*GetOwnerScaleFactor));
      DrawButtonText(ACanvas, FCommandLinkHint, taLeftJustify, FSpacing, CalcMaxBorderWidth,
        LTextRect, LTextFlags, False);
    end;
  end
  else
    DrawButtonText(ACanvas, LCaption, FCaptionAlignment, FSpacing, CalcMaxBorderWidth,
      LTextRect, LTextFlags);
end;

procedure TStyledButtonRender.DrawButton(const ACanvas: TCanvas;
  const AEraseBackground: Boolean);
var
  LSurfaceRect: TRect;
  LOldFontName: TFontName;
  LOldFontColor: TColor;
  LOldFontStyle: TFontStyles;
  LOldParentFont: boolean;
begin
  if not (csDesigning in ComponentState) and
   (not FOwnerControl.Visible or (FUpdateCount > 0)) then
    Exit;

  LOldParentFont := ParentFont;
  LOldFontName := Font.Name;
  LOldFontColor := Font.Color;
  LOldFontStyle := Font.Style;

  try
    DrawBackgroundAndBorder(ACanvas, AEraseBackground);

    LSurfaceRect := FOwnerControl.ClientRect;
    if FDropDownRect.Width <> 0 then
      Dec(LSurfaceRect.Right, FDropDownRect.Width);

    DrawCaptionAndImage(ACanvas, LSurfaceRect);

    LSurfaceRect := FOwnerControl.ClientRect;
    DrawNotificationBadge(ACanvas, LSurfaceRect);
  finally
    if LOldParentFont then
      ParentFont := LOldParentFont
    else
    begin
      Font.Name := LOldFontName;
      Font.Color := LOldFontColor;
      Font.Style := LOldFontStyle;
    end;
  end;
end;

function TStyledButtonRender.GetBackGroundColor: TColor;
begin
  Result := GetAttributes(ButtonState).ButtonColor;
end;

function TStyledButtonRender.GetButtonState: TStyledButtonState;
begin
  if FOwnerControl is TCustomStyledGraphicButton then
    Result := TCustomStyledGraphicButton(FOwnerControl).ButtonState
  else if FOwnerControl is TCustomStyledButton then
    Result := TCustomStyledButton(FOwnerControl).ButtonState
  else
    Result := bsmNormal;
end;

function TStyledButtonRender.GetDrawingStyle(const ACanvas: TCanvas;
  const AButtonState: TStyledButtonState): TStyledButtonAttributes;
begin
  //Getting drawing styles
  Result := GetAttributes(AButtonState);
  ACanvas.Pen.Style := Result.PenStyle;
  ACanvas.Pen.Width := Round(Result.BorderWidth{$IFDEF D10_3+}*FOwnerControl.ScaleFactor{$ENDIF});
  ACanvas.Pen.Color := Result.BorderColor;
  ACanvas.Brush.Style := Result.BrushStyle;
  if Result.ButtonDrawStyle <> btnClear then
    ACanvas.Brush.Color := Result.ButtonColor;
  ACanvas.Font := Font;
  ACanvas.Font.Color := Result.FontColor;
  if ParentFont then
    ACanvas.Font.Style := Result.FontStyle;

  if FStyle = TCustomButton.TButtonStyle.bsSplitButton then
  begin
    FDropDownRect := FOwnerControl.ClientRect;
    FDropDownRect.Left := FDropDownRect.Right - GetSplitButtonWidth - ACanvas.Pen.Width -2;
  end
  else
    FDropDownRect.Width := 0;
end;

function TStyledButtonRender.GetFocused: Boolean;
begin
  if FOwnerControl is TCustomControl then
    Result := TCustomControl(FOwnerControl).Focused
  else
    Result := False;
end;

function TStyledButtonRender.GetRescalingButton: Boolean;
begin
  Result := FRescalingButton;
end;

procedure TStyledButtonRender.SetRescalingButton(const AValue: Boolean);
begin
  FRescalingButton := AValue;
end;

procedure TStyledButtonRender.SetStyleDrawType(const AValue: TStyledButtonDrawType);
begin
  if FStyleDrawType <> AValue then
  begin
    FStyleDrawType := AValue;
    FCustomDrawType := True;
    Invalidate;
  end;
end;

function TStyledButtonRender.IsDefaultImageMargins: Boolean;
begin
  //Default image margins: when all margins are zero except for
  //Actual ImageAlignment margin is default
  case FImageAlignment of
    iaLeft: Result := (FImageMargins.Left = DEFAULT_IMAGE_HMARGIN) and
      (FImageMargins.Top = 0) and (FImageMargins.Right = 0) and (FImageMargins.Bottom = 0);
    iaRight: Result := (FImageMargins.Right = DEFAULT_IMAGE_HMARGIN) and
      (FImageMargins.Top = 0) and (FImageMargins.Left = 0) and (FImageMargins.Bottom = 0);
    iaTop: Result := (FImageMargins.Top = DEFAULT_IMAGE_VMARGIN) and
      (FImageMargins.Left = 0) and (FImageMargins.Right = 0) and (FImageMargins.Bottom = 0);
    iaBottom: Result := (FImageMargins.Bottom = DEFAULT_IMAGE_VMARGIN) and
      (FImageMargins.Left = 0) and (FImageMargins.Right = 0) and (FImageMargins.Top = 0);
    iaCenter: Result := (FImageMargins.Bottom = 0) and
      (FImageMargins.Left = 0) and (FImageMargins.Right = 0) and (FImageMargins.Top = 0);
  else
    Result := False;
  end;
end;

procedure TStyledButtonRender.CalcDefaultImageMargins(const AValue: TImageAlignment);

  function AdJustMargin(const AMargin, AOffset: Integer): Integer;
  begin
    Result := AMargin + Round(AOffset*GetOwnerScaleFactor);
  end;

begin
  if IsDefaultImageMargins then
  begin
    FImageMargins.Left := 0;
    FImageMargins.Right := 0;
    FImageMargins.Top := 0;
    FImageMargins.Bottom := 0;
    case AValue of
      iaLeft: FImageMargins.Left := AdJustMargin(FImageMargins.Left, DEFAULT_IMAGE_HMARGIN);
      iaRight: FImageMargins.Right := AdJustMargin(FImageMargins.Right, DEFAULT_IMAGE_HMARGIN);
      iaTop: FImageMargins.Top := AdJustMargin(FImageMargins.Top, DEFAULT_IMAGE_VMARGIN);
      iaBottom: FImageMargins.Bottom := AdJustMargin(FImageMargins.Bottom, DEFAULT_IMAGE_VMARGIN);
    end;
  end;
end;

procedure TStyledButtonRender.SetLayout(const AValue: TButtonLayout);
begin
  if AValue <> FButtonLayout then
  begin
    FButtonLayout := AValue;
    Invalidate;
  end;
end;

procedure TStyledButtonRender.SetImageAlignment(const AValue: TImageAlignment);
begin
  if AValue <> FImageAlignment then
  begin
    CalcDefaultImageMargins(AValue);
    FImageAlignment := AValue;
    Invalidate;
  end;
end;

procedure TStyledButtonRender.SetDisabledImageIndex(const AValue: TImageIndex);
begin
  if AValue <> FDisabledImageIndex then
  begin
    FDisabledImageIndex := AValue;
    {$IFDEF D10_4+}
    if (FDisabledImages <> nil) and FDisabledImages.IsImageNameAvailable then
      FDisabledImageName := FDisabledImages.GetNameByIndex(FDisabledImageIndex)
    else
      if (FDisabledImages = nil) and (FImages <> nil) and FImages.IsImageNameAvailable then
        FDisabledImageName := FImages.GetNameByIndex(FDisabledImageIndex);
    {$ENDIF}
    UpdateImageIndexAndName;
  end;
end;

function TStyledButtonRender.GetImageIndex: TImageIndex;
begin
  if (FOwnerControl is TCustomStyledButton) then
    Result := TCustomStyledButton(FOwnerControl).ImageIndex
  else if (FOwnerControl is TCustomStyledGraphicButton) then
    Result := TCustomStyledGraphicButton(FOwnerControl).ImageIndex
  else
    raise EStyledButtonError.Create(ERROR_CANNOT_USE_RENDER);
end;

procedure TStyledButtonRender.SetImageIndex(const AValue: TImageIndex);
begin
  if AValue <> FImageIndex then
  begin
    FImageIndex := AValue;
    if (FOwnerControl is TCustomStyledButton) then
      TCustomStyledButton(FOwnerControl).ImageIndex := AValue
    else if (FOwnerControl is TCustomStyledGraphicButton) then
      TCustomStyledGraphicButton(FOwnerControl).ImageIndex := AValue;

    {$IFDEF D10_4+}
    if (FImages <> nil) and FImages.IsImageNameAvailable then
       ImageName := FImages.GetNameByIndex(ImageIndex);
    {$ENDIF}
    UpdateImageIndexAndName;
    Invalidate;
  end;
end;

{$IFDEF D10_4+}
procedure TStyledButtonRender.SetDisabledImageName(const AValue: TImageName);
begin
  if AValue <> FDisabledImageName then
  begin
    FDisabledImageName := AValue;
    if (FDisabledImages <> nil) and FDisabledImages.IsImageNameAvailable then
      FDisabledImageIndex := FDisabledImages.GetIndexByName(FDisabledImageName)
    else
      if (FDisabledImages = nil) and (FImages <> nil) and FImages.IsImageNameAvailable then
        FDisabledImageIndex := FImages.GetIndexByName(FDisabledImageName);
    UpdateImageIndexAndName;
  end;
end;

function TStyledButtonRender.GetImageName: TImageName;
begin
  if (FOwnerControl is TCustomStyledButton) then
    Result := TCustomStyledButton(FOwnerControl).FImageName
  else if (FOwnerControl is TCustomStyledGraphicButton) then
    Result := TCustomStyledGraphicButton(FOwnerControl).FImageName
  else
    raise EStyledButtonError.Create(ERROR_CANNOT_USE_RENDER);
end;

procedure TStyledButtonRender.SetImageName(const AValue: TImageName);
begin
  if AValue <> FImageName then
  begin
    FImageName := AValue;
    if (FOwnerControl is TCustomStyledButton) then
      TCustomStyledButton(FOwnerControl).ImageName := AValue
    else if (FOwnerControl is TCustomStyledGraphicButton) then
      TCustomStyledGraphicButton(FOwnerControl).ImageName := AValue;
    UpdateImageIndexAndName;
    Invalidate;
  end;
end;

procedure TStyledButtonRender.UpdateImages;
begin
  if CheckWin32Version(5, 1) and (ImageIndex <> -1) then
  begin
    CheckImageIndexes;
  end;
end;

procedure TStyledButtonRender.UpdateImageName(Index: TImageIndex;
  var Name: TImageName);
begin
  if (FImages <> nil) and FImages.IsImageNameAvailable then
      Name := FImages.GetNameByIndex(Index);
  UpdateImageIndexAndName;
end;

procedure TStyledButtonRender.UpdateImageIndex(Name: TImageName;
  var Index: TImageIndex);
begin
  if (FImages <> nil) and FImages.IsImageNameAvailable then
      Index := FImages.GetIndexByName(Name);
  UpdateImageIndexAndName;
end;

procedure TStyledButtonRender.SetHotImageName(const AValue: TImageName);
begin
  if AValue <> FHotImageName then
  begin
    FHotImageName := AValue;
    UpdateImageIndex(AValue, FHotImageIndex);
  end;
end;

procedure TStyledButtonRender.SetStylusHotImageName(const AValue: TImageName);
begin
  if AValue <> FStylusHotImageName then
  begin
    FStylusHotImageName := AValue;
    UpdateImageIndex(AValue, FStylusHotImageIndex);
  end;
end;

procedure TStyledButtonRender.SetPressedImageName(const AValue: TImageName);
begin
  if AValue <> FPressedImageName then
  begin
    FPressedImageName := AValue;
    UpdateImageIndex(AValue, FPressedImageIndex);
  end;
end;

procedure TStyledButtonRender.SetSelectedImageName(const AValue: TImageName);
begin
  if AValue <> FSelectedImageName then
  begin
    FSelectedImageName := AValue;
    UpdateImageIndex(AValue, FSelectedImageIndex);
  end;
end;
{$ENDIF}

procedure TStyledButtonRender.UpdateImageIndexAndName;
begin
{$IFDEF D10_4+}
  if (FImages <> nil) and FImages.IsImageNameAvailable then
  begin
    if (ImageName <> '') and (ImageIndex = -1) then
      ImageIndex := FImages.GetIndexByName(ImageName)
    else if (ImageName = '') and (ImageIndex <> -1) then
      ImageName := FImages.GetNameByIndex(ImageIndex);
    CheckImageIndexes;
  end;
{$ENDIF}
end;

function TStyledButtonRender.GetActiveStyleName: string;
begin
  Result := Vcl.ButtonStylesAttributes.GetActiveStyleName(FOwnerControl);
end;

procedure TStyledButtonRender.SetDisabledImages(const AValue: TCustomImageList);
begin
  if AValue <> FDisabledImages then
  begin
    if DisabledImages <> nil then
    begin
      DisabledImages.RemoveFreeNotification(FOwnerControl);
      DisabledImages.UnRegisterChanges(FImageChangeLink);
    end;
    FDisabledImages := AValue;
    if DisabledImages <> nil then
    begin
      DisabledImages.RegisterChanges(FImageChangeLink);
      DisabledImages.FreeNotification(FOwnerControl);
    end;
    UpdateImageIndexAndName;
    Invalidate;
  end;
end;

procedure TStyledButtonRender.UpAllButtons;
var
  LParent: TWinControl;
  LControl: TControl;
  I: Integer;
  LGraphicButton: TCustomStyledGraphicButton;
  LButton: TCustomStyledButton;
begin
  LParent := FOwnerControl.Parent;
  for I := 0 to LParent.ControlCount -1 do
  begin
    LControl := LParent.Controls[I];
    if LControl is TCustomStyledGraphicButton then
    begin
      LGraphicButton := TCustomStyledGraphicButton(LControl);
      if LGraphicButton.Down and (LGraphicButton <> FOwnerControl) and
        (LGraphicButton.GroupIndex = GroupIndex) then
      begin
        LGraphicButton.Down := False;
        Break;
      end;
    end;
    if LControl is TCustomStyledButton then
    begin
      LButton := TCustomStyledButton(LControl);
      if LButton.Down and (LButton <> FOwnerControl) and
        (LButton.GroupIndex = GroupIndex) then
      begin
        LButton.Down := False;
        Break;
      end;
    end;
  end;
end;

procedure TStyledButtonRender.SetDown(const AValue: Boolean);
begin
  if AValue <> FDown then
  begin
    FDown := AValue;
    if AValue then
    begin
      FState := bsDown;
      UpAllButtons;
    end
    else
      FState := bsUp;
    Invalidate;
  end;
end;

procedure TStyledButtonRender.SetDropDownMenu(const AValue: TPopupMenu);
begin
  if AValue <> FDropDownMenu then
  begin
    if DropDownMenu <> nil then
      DropDownMenu.RemoveFreeNotification(FOwnerControl);
    FDropDownMenu := AValue;
    if DropDownMenu <> nil then
      DropDownMenu.FreeNotification(FOwnerControl);
  end;
end;

procedure TStyledButtonRender.SetElevationRequired(const AValue: Boolean);
begin
  if FElevationRequired <> AValue then
  begin
    FElevationRequired := AValue;
    Invalidate;
  end;
end;

procedure TStyledButtonRender.SetFlat(const AValue: Boolean);
begin
  if FFlat <> AValue then
  begin
    FFlat := AValue;
    Invalidate;
  end;
end;

procedure TStyledButtonRender.SetFocus;
begin
  if FOwnerControl is TCustomControl and
    TCustomControl(FOwnerControl).CanFocus and
    TCustomControl(FOwnerControl).TabStop then
    TCustomControl(FOwnerControl).SetFocus;
end;

procedure TStyledButtonRender.SetGlyph(const AValue: TBitmap);
begin
  if Assigned(AValue) then
    FTransparentColor := AValue.TransparentColor;
  Glyph.Assign(AValue);
  Invalidate;
end;

procedure TStyledButtonRender.SetGroupIndex(const AValue: Integer);
begin
  if FGroupIndex <> AValue then
  begin
    FGroupIndex := AValue;
    Invalidate;
  end;
end;

procedure TStyledButtonRender.SetHotImageIndex(const AValue: TImageIndex);
begin
  if AValue <> FHotImageIndex then
  begin
    FHotImageIndex := AValue;
    {$IFDEF D10_4+}
    UpdateImageName(AValue, FHotImageName);
    {$ENDIF}
  end;
end;

procedure TStyledButtonRender.SetStylusHotImageIndex(const AValue: TImageIndex);
begin
  if FStylusHotImageIndex <> AValue then
  begin
    FStylusHotImageIndex := AValue;
    {$IFDEF D10_4+}
    UpdateImageName(AValue, FHotImageName);
    {$ENDIF}
  end;
end;


procedure TStyledButtonRender.SetImages(const AValue: TCustomImageList);
begin
  if AValue <> FImages then
  begin
    if FImages <> nil then
    begin
      FImages.RemoveFreeNotification(FOwnerControl);
      FImages.UnRegisterChanges(FImageChangeLink);
    end;
    FImages := AValue;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(FOwnerControl);
    end;
    UpdateImageIndexAndName;
    Invalidate;
  end;
end;

procedure TStyledButtonRender.SetKind(const AValue: TBitBtnKind);
begin
  if AValue <> FKind then
  begin
    if AValue <> bkCustom then
    begin
      Default := AValue in [bkOK, bkYes];
      Cancel := AValue in [bkCancel, bkNo];

      if ((csLoading in ComponentState) and (Caption = '')) or
        (not (csLoading in ComponentState)) then
      begin
        if AValue <> bkCustom then
          Caption := BitBtnCaptions(AValue);
      end;

      ModalResult := BitBtnModalResults[AValue];
    end;
    FKind := AValue;
    Invalidate;
  end;
end;

function TStyledButtonRender.UpdateStyleUsingModalResult: boolean;
begin
  if (FModalResult <> mrNone) then
  begin
    Result := True;
    //Force style of the button as defined into Family
    StyleFamilyUpdateAttributesByModalResult(FModalResult,
      FStyleFamily, FStyleClass, FStyleAppearance);
    StyleApplied := ApplyButtonStyle;
  end
  else
    Result := False;
end;

procedure TStyledButtonRender.SetMargin(const AValue: Integer);
begin
  if FMargin <> AValue then
  begin
    FMargin := AValue;
    Invalidate;
  end;
end;

procedure TStyledButtonRender.SetModalResult(const AValue: TModalResult);
begin
  if FModalResult <> AValue then
  begin
    FModalResult := AValue;
    UpdateStyleUsingModalResult;
  end;
end;

procedure TStyledButtonRender.SetNumGlyphs(const AValue: TNumGlyphs);
var
  LValue: TNumGlyphs;
begin
  LValue := AValue;
  if LValue < 0 then LValue := 1
  else if LValue > 4 then LValue := 4;
  if LValue <> FNumGlyphs then
  begin
    FNumGlyphs := LValue;
    Invalidate;
  end;
end;

procedure TStyledButtonRender.SetPressedImageIndex(const AValue: TImageIndex);
begin
  if AValue <> FPressedImageIndex then
  begin
    FPressedImageIndex := AValue;
    {$IFDEF D10_4+}
    UpdateImageName(AValue, FPressedImageName);
    {$ENDIF}
  end;
end;

procedure TStyledButtonRender.SetButtonStyleNormal(const AValue: TStyledButtonAttributes);
begin
  if not SameStyledButtonStyle(FButtonStyleNormal, AValue) then
  begin
    FButtonStyleNormal := AValue;
  end;
end;

procedure TStyledButtonRender.SetButtonStyle(
  const AStyleFamily: TStyledButtonFamily;
  const AStyleClass: TStyledButtonClass;
  const AStyleAppearance: TStyledButtonAppearance);
begin
  FStyleFamily := AStyleFamily;
  FStyleClass := AStyleClass;
  FStyleAppearance := AStyleAppearance;
  if not ApplyButtonStyle then
    raise EStyledButtonError.CreateFmt(ERROR_SETTING_BUTTON_STYLE,
      [AStyleFamily, AStyleClass, AStyleAppearance]);
end;

procedure TStyledButtonRender.SetButtonStyle(const AStyleFamily: TStyledButtonFamily;
  const AModalResult: TModalResult);
begin
  FStyleFamily := AStyleFamily;
  FModalResult := AModalResult;
  StyleFamilyUpdateAttributesByModalResult(FModalResult,
    FStyleFamily, FStyleClass, FStyleAppearance);
  if not ApplyButtonStyle then
    raise EStyledButtonError.CreateFmt(ERROR_SETTING_BUTTON_STYLE,
      [FStyleFamily, FStyleClass, FStyleAppearance]);
end;

procedure TStyledButtonRender.SetButtonStyleDisabled(
  const AValue: TStyledButtonAttributes);
begin
  if not SameStyledButtonStyle(FButtonStyleDisabled, AValue) then
  begin
    FButtonStyleDisabled := AValue;
  end;
end;

procedure TStyledButtonRender.SetButtonStylePressed(const AValue: TStyledButtonAttributes);
begin
  if not SameStyledButtonStyle(FButtonStylePressed, AValue) then
  begin
    FButtonStylePressed := AValue;
  end;
end;

procedure TStyledButtonRender.SetButtonStyleSelected(const AValue: TStyledButtonAttributes);
begin
  if not SameStyledButtonStyle(FButtonStyleSelected, AValue) then
  begin
    FButtonStyleSelected := AValue;
  end;
end;

procedure TStyledButtonRender.SetButtonStyleHot(const AValue: TStyledButtonAttributes);
begin
  if not SameStyledButtonStyle(FButtonStyleHot, AValue) then
  begin
    FButtonStyleHot := AValue;
  end;
end;

procedure TStyledButtonRender.SetNotificationBadge(
  const AValue: TNotificationBadgeAttributes);
begin
  if not SameNotificationBadgeAttributes(FNotificationBadge, AValue) then
  begin
    FNotificationBadge := AValue;
  end;
end;

procedure TStyledButtonRender.SetImageMargins(const AValue: TImageMargins);
begin
  FImageMargins.Assign(AValue);
end;

procedure TStyledButtonRender.SetStyleRadius(const AValue: Integer);
begin
  if FStyleRadius <> AValue then
  begin
    if AValue <= 0 then
      raise EReadError.create(SInvalidProperty);
    FStyleRadius := AValue;
    Invalidate;
  end;
end;

procedure TStyledButtonRender.SetStyleRoundedCorners(const AValue: TRoundedCorners);
begin
  if FStyleRoundedCorners <> AValue then
  begin
    FStyleRoundedCorners := AValue;
    Invalidate;
  end;
end;

function TStyledButtonRender.GetHasCustomAttributes: Boolean;
begin
  Result := FButtonStyleNormal.HasCustomAttributes or
    FButtonStylePressed.HasCustomAttributes or
    FButtonStyleSelected.HasCustomAttributes or
    FButtonStyleHot.HasCustomAttributes or
    FButtonStyleDisabled.HasCustomAttributes;
end;

procedure TStyledButtonRender.SetHasCustomAttributes(const AValue: Boolean);
begin
  if not AValue then
  begin
    FButtonStyleNormal.ResetCustomAttributes;
    FButtonStylePressed.ResetCustomAttributes;
    FButtonStyleSelected.ResetCustomAttributes;
    FButtonStyleHot.ResetCustomAttributes;
    FButtonStyleDisabled.ResetCustomAttributes;
  end;
end;

procedure TStyledButtonRender.SetSelectedImageIndex(const AValue: TImageIndex);
begin
  if AValue <> FSelectedImageIndex then
  begin
    FSelectedImageIndex := AValue;
    {$IFDEF D10_4+}
    UpdateImageName(AValue, FSelectedImageName);
    {$ENDIF}
  end;
end;

procedure TStyledButtonRender.SetShowCaption(const AValue: Boolean);
begin
  if FShowCaption <> AValue then
  begin
    FShowCaption := AValue;
    Invalidate;
  end;
end;

procedure TStyledButtonRender.SetSpacing(const AValue: Integer);
begin
  if FSpacing <> AValue then
  begin
    FSpacing := AValue;
    Invalidate;
  end;
end;

procedure TStyledButtonRender.SetState(const AValue: TButtonState);
begin
  if FState <> AValue then
  begin
    if (AValue <> bsDown) and FDown and not FAllowAllUp then
      Exit;
    FState := AValue;
  end;
end;

procedure TStyledButtonRender.SetStyle(const AValue: TCustomButton.TButtonStyle);
const
  DefCmdLinkWidth = 175;
  DefCmdLinkHeights: array[Boolean] of Integer = (57, 41);
begin
  if AValue <> FStyle then
  begin
    FStyle := AValue;
    if not (csLoading in ComponentState) then
    begin
      case AValue of
        bsPushButton,
        bsSplitButton:
          begin
            if FStyle = bsCommandLink then
              FOwnerControl.SetBounds(FOwnerControl.Left, FOwnerControl.Top,
                FOwnerControl.ExplicitWidth, FOwnerControl.ExplicitHeight);
          end;
        bsCommandLink:
          begin
            if Height < DefCmdLinkHeights[FCommandLinkHint = ''] then
              FOwnerControl.Height := DefCmdLinkHeights[FCommandLinkHint = ''];
            if Width < DefCmdLinkWidth then
              FOwnerControl.Width := DefCmdLinkWidth;
            FStyle := AValue;
          end;
      end;
    end;
    if (FStyle = TCustomButton.TButtonStyle.bsCommandLink) then
      CaptionAlignment := TAlignment.taLeftJustify;
    FMouseOverDropDown := False;
    if not (csLoading in ComponentState) then
      Invalidate;
  end;
end;

procedure TStyledButtonRender.SetStyleAppearance(
  const AValue: TStyledButtonAppearance);
var
  LValue: TStyledButtonAppearance;
begin
  LValue := AValue;
  if LValue = '' then
    LValue := DEFAULT_APPEARANCE;
  if (LValue <> Self.FStyleAppearance) or not FStyleApplied then
  begin
    Self.FStyleAppearance := LValue;
    StyleApplied := ApplyButtonStyle;
  end;
end;

procedure TStyledButtonRender.SetStyleApplied(const AValue: Boolean);
begin
  FStyleApplied := AValue;
end;

procedure TStyledButtonRender.SetStyleClass(
  const AValue: TStyledButtonClass);
var
  LValue: TStyledButtonClass;
begin
  LValue := AValue;
  //Using a specific Class in Classic Family force
  //StyleElements without seClient
  if (FStyleFamily = DEFAULT_CLASSIC_FAMILY) then
  begin
    if (LValue <> DEFAULT_WINDOWS_CLASS) then
      FOwnerControl.StyleElements := FOwnerControl.StyleElements - [seClient];
//    else
//      LValue := GetActiveStyleName;
    if LValue = '' then
      LValue := DEFAULT_WINDOWS_CLASS;
  end;
  if (LValue <> Self.FStyleClass) or not FStyleApplied then
  begin
    Self.FStyleClass := LValue;
    StyleApplied := ApplyButtonStyle;
  end;
end;

procedure TStyledButtonRender.SetStyleFamily(
  const AValue: TStyledButtonFamily);
var
  LValue: TStyledButtonFamily;
begin
  LValue := AValue;
  if LValue = '' then
    LValue := DEFAULT_CLASSIC_FAMILY;
  if (LValue <> Self.FStyleFamily) or not FStyleApplied then
  begin
    FStyleFamily := LValue;
    StyleApplied := ApplyButtonStyle or
      UpdateStyleUsingModalResult;
  end;
  if FStyleFamily = DEFAULT_CLASSIC_FAMILY then
    FOwnerControl.StyleElements := [seFont, seClient, seBorder];
end;

procedure TStyledButtonRender.SetText(const AValue: TCaption);
begin
  if Caption <> AValue then
  begin
    FSetCaption(AValue);
    Invalidate;
  end;
end;

procedure TStyledButtonRender.SetCaptionAlignment(const AValue: TAlignment);
begin
  if FCaptionAlignment <> AValue then
  begin
    FCaptionAlignment := AValue;
    Invalidate;
  end;
end;

procedure TStyledButtonRender.SetTransparent(const AValue: Boolean);
begin
  if FTransparent <> AValue then
  begin
    FTransparent := AValue;
    if AValue then
      FOwnerControl.ControlStyle := FOwnerControl.ControlStyle - [csOpaque] else
      FOwnerControl.ControlStyle := FOwnerControl.ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TStyledButtonRender.SetWordWrap(const AValue: Boolean);
begin
  if FWordWrap <> AValue then
  begin
    FWordWrap := AValue;
    Invalidate;
  end;
end;

procedure TStyledButtonRender.Loaded;
begin
  inherited;
  SetImageIndex(ImageIndex);
  if not FStyleApplied (*and not HasCustomAttributes*) then
  begin
    StyleFamilyUpdateAttributesByModalResult(FModalResult,
      FStyleFamily, FStyleClass, FStyleAppearance);
    StyleApplied := ApplyButtonStyle;
  end;
end;

procedure TStyledButtonRender.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then
  begin
    State := bsDown;
    SetFocus;
    Invalidate;
    inherited;
  end;
end;

function TStyledButtonRender.GetSplitButtonWidth: Integer;
begin
  Result := Round(12{$IFDEF D10_3+}* FOwnerControl.ScaleFactor{$ENDIF});
end;

procedure TStyledButtonRender.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouseOverDropDown := (FStyle = TCustomButton.TButtonStyle.bsSplitButton)
    and (X >= FDropDownRect.Left);
end;

procedure TStyledButtonRender.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then
  begin
    if GroupIndex <> 0 then
    begin
      if AllowAllUp and Down then
        Down := False
      else
        Down := True;
    end
    else
      State := bsUp;
    Invalidate;
  end;
end;

function TStyledButtonRender.GetAction: TCustomAction;
begin
  if FOwnerControl.Action is TCustomAction then
    Result := TCustomAction(FOwnerControl.Action)
  else
    Result := nil;
end;

procedure TStyledButtonRender.SetAction(const AAction: TCustomAction);
begin
  FOwnerControl.Action := AAction;
end;

procedure TStyledButtonRender.SetAllowAllUp(const AValue: Boolean);
begin
  if FAllowAllUp <> AValue then
  begin
    FAllowAllUp := AValue;
    Invalidate;
  end;
end;

function TStyledButtonRender.GetName: TComponentName;
begin
  Result := FOwnerControl.Name;
end;

function TStyledButtonRender.GetNumGlyphs: TNumGlyphs;
begin
  Result := FNumGlyphs;
end;

function TStyledButtonRender.GetOwnerScaleFactor: Single;
begin
  Result := {$IFDEF D10_3+}FOwnerControl.ScaleFactor{$ELSE}1{$ENDIF};
end;

procedure TCustomStyledGraphicButton.SetNumGlyphs(const AValue: TNumGlyphs);
begin
  FRender.NumGlyphs := AValue;
end;

function TStyledButtonRender.GetParent: TWinControl;
begin
  Result := FOwnerControl.Parent;
end;

procedure TStyledButtonRender.SetParent(const AValue: TWinControl);
begin
  FOwnerControl.Parent := AValue;
end;

function TStyledButtonRender.GetControlEnabled: Boolean;
begin
  Result := FOwnerControl.Enabled;
end;

procedure TStyledButtonRender.SetCommandLinkHint(const AValue: string);
begin
  if FCommandLinkHint <> AValue then
  begin
    FCommandLinkHint := AValue;
    Invalidate;
  end;
end;

procedure TStyledButtonRender.SetControlEnabled(const AValue: Boolean);
begin
  FOwnerControl.Enabled := AValue;
end;

procedure TStyledButtonRender.SetCustomStyleDrawType(
  ACustomStyleDrawType: Boolean);
begin
  FCustomDrawType := ACustomStyleDrawType;
end;

function TStyledButtonRender.GetParentFont: Boolean;
begin
  Assert(Assigned(FGetParentFont));
  Result := FGetParentFont;
end;

procedure TStyledButtonRender.SetParentFont(const AValue: Boolean);
begin
  Assert(Assigned(FSetParentFont));
  FSetParentFont(AValue);
end;

function TStyledButtonRender.GetFont: TFont;
begin
  Assert(Assigned(FControlFont));
  FControlFont(Result);
end;

function TStyledButtonRender.IsGlyphAssigned: Boolean;
begin
  Result := Assigned(FGlyph) and
    (FGlyph.Width <> 0) and (FGlyph.Height <> 0);
end;

function TStyledButtonRender.GetGlyph: TBitmap;
begin
  if not Assigned(FGlyph) then
  begin
    FGlyph := TBitmap.Create;
    FGlyph.Width := 0;
    FGlyph.Height := 0;
  end;
  Result := FGlyph;
end;

function TStyledButtonRender.GetHint: string;
begin
  if FOwnerControl is TGraphicControl then
    Result := TGraphicControl(FOwnerControl).Hint
  else if FOwnerControl is TCustomControl then
    Result := TCustomControl(FOwnerControl).Hint
  else
    Result := '';
end;

function TStyledButtonRender.GetComponentState: TComponentState;
begin
  Result := FOwnerControl.ComponentState;
end;

function TStyledButtonRender.GetComponentWidth: Integer;
begin
  Result := FOwnerControl.Width;
end;

function TStyledButtonRender.GetCaptionToDraw: string;
begin
  if FOwnerControl is TCustomStyledGraphicButton then
    Result := TCustomStyledGraphicButton(FOwnerControl).GetCaptionToDraw
  else if FOwnerControl is TCustomStyledButton then
    Result := TCustomStyledButton(FOwnerControl).GetCaptionToDraw
  else
    Result := '';
end;

function TStyledButtonRender.GetComponentHeight: Integer;
begin
  Result := FOwnerControl.Height;
end;

procedure TStyledButtonRender.InternalCopyImage(Image: TBitmap;
  ImageList: TCustomImageList; Index: Integer);
begin
  with Image do
  begin
    Width := ImageList.Width;
    Height := ImageList.Height;
    Canvas.Brush.Color := clFuchsia;
    Canvas.FillRect(Rect(0,0, Width, Height));
    ImageList.Draw(Canvas, 0, 0, Index);
  end;
end;

procedure TStyledButtonRender.Invalidate;
begin
  if not (csLoading in FOwnerControl.ComponentState) then
    FOwnerControl.Invalidate;
end;

{ TCustomStyledGraphicButton }

procedure TCustomStyledGraphicButton.AssignStyleTo(ADestRender: TStyledButtonRender);
begin
  FRender.AssignStyleTo(ADestRender);
end;

procedure TCustomStyledGraphicButton.AssignStyleTo(ADest: TCustomStyledGraphicButton);
begin
  FRender.AssignStyleTo(ADest.Render);
end;

function TCustomStyledGraphicButton.AssignAttributes(
  const AEnabled: Boolean = True;
  const AImageList: TCustomImageList = nil;
  {$IFDEF D10_4+}const AImageName: string = '';{$ENDIF}
  const AImageIndex: Integer = -1;
  const AImageAlignment: TImageAlignment = iaLeft;
  const AAction: TCustomAction = nil;
  const AOnClick: TNotifyEvent = nil;
  const AName: string = ''): TCustomStyledGraphicButton;
begin
  Result := FRender.AssignAttributes(AEnabled,
    AImageList,
    {$IFDEF D10_4+}AImageName,{$ENDIF}
    AImageIndex,
    AImageAlignment,
    AAction,
    AOnClick,
    AName) as TCustomStyledGraphicButton;
end;

procedure TCustomStyledGraphicButton.AssignTo(ADest: TPersistent);
var
  LDest: TCustomStyledGraphicButton;
begin
  inherited AssignTo(ADest);
  if ADest is TCustomStyledGraphicButton then
  begin
    LDest := TCustomStyledGraphicButton(ADest);
    FRender.AssignStyleTo(LDest.Render);
    LDest.Cursor := Self.Cursor;
    LDest.Hint := Self.Hint;
    LDest.Visible := Self.Visible;
    LDest.Caption := Self.Caption;
    LDest.ModalResult := Self.ModalResult;
    LDest.Tag := Self.Tag;
    LDest.Enabled := Self.Enabled;
    LDest.Down := Self.Down;
    LDest.AllowAllUp := Self.AllowAllUp;
  end;
end;

procedure TCustomStyledGraphicButton.BeginUpdate;
begin
  FRender.BeginUpdate;
end;

procedure TCustomStyledGraphicButton.EndUpdate;
begin
  FRender.EndUpdate;
end;

{$IFDEF HiDPISupport}
procedure TCustomStyledGraphicButton.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  if isDpiChange then
  begin
    RescalingButton := True;
    try
      if Assigned(FRender) then
        FRender.ChangeScale(M, D, isDpiChange);
      inherited;
    finally
      RescalingButton := False;
    end;
  end
  else
    inherited;
end;
{$ENDIF}

procedure TCustomStyledGraphicButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Visible then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TCustomStyledGraphicButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FRender.CMEnabledChanged(Message);
end;

procedure TCustomStyledGraphicButton.CMEnter(var Message: TCMEnter);
begin
  inherited;
  FRender.CMEnter(Message);
end;

procedure TCustomStyledGraphicButton.CMMouseEnter(var Message: TNotifyEvent);
begin
  inherited;
  FRender.CMMouseEnter(Message);
end;

procedure TCustomStyledGraphicButton.CMMouseLeave(var Message: TNotifyEvent);
begin
  inherited;
  FRender.CMMouseLeave(Message);
end;

procedure TCustomStyledGraphicButton.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  FRender.CMStyleChanged(Message);
end;

procedure TCustomStyledGraphicButton.Click;
begin
  FRender.Click(False);
end;

constructor TCustomStyledGraphicButton.CreateStyled(AOwner: TComponent;
  const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance);
begin
  inherited Create(AOwner);
  FImageIndex := -1;
  FRender := GetRenderClass.CreateStyled(Self,
    ControlClick, ControlFont, GetCaption, SetCaption,
      GetParentFont, SetParentFont,
      AFamily, AClass, AAppearance,
      _DefaultStyleDrawType, _DefaultCursor, _UseCustomDrawType);
end;

constructor TCustomStyledGraphicButton.Create(AOwner: TComponent);
begin
  CreateStyled(AOwner,
    _DefaultFamily,
    _DefaultClass,
    _DefaultAppearance);
end;

constructor TCustomStyledGraphicButton.CreateStyled(AOwner: TComponent;
  const AFamily: TStyledButtonFamily; const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  const ADrawType: TStyledButtonDrawType;
  const ACursor: TCursor;
  const AUseCustomDrawType: Boolean);
begin
  inherited Create(AOwner);
  FImageIndex := -1;
  FRender := GetRenderClass.CreateStyled(Self,
    ControlClick, ControlFont, GetCaption, SetCaption,
      GetParentFont, SetParentFont,
      AFamily, AClass, AAppearance,
      ADrawType, ACursor, AUseCustomDrawType);
end;

procedure TCustomStyledGraphicButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited;
  FRender.ActionChange(Sender, CheckDefaults);
end;

function TCustomStyledGraphicButton.IsCaptionAlignmentStored: Boolean;
begin
  Result := FRender.IsCaptionAlignmentStored;
end;

destructor TCustomStyledGraphicButton.Destroy;
begin
  FreeAndNil(FRender);
  inherited Destroy;
end;

procedure TCustomStyledGraphicButton.DoDropDownMenu;
begin
  FRender.DoDropDownMenu;
end;

function TCustomStyledGraphicButton.GetText: TCaption;
begin
  Result := FRender.GetText;
end;

function TCustomStyledGraphicButton.GetTransparent: Boolean;
begin
  Result := FRender.Transparent;
end;

function TCustomStyledGraphicButton.GetKind: TBitBtnKind;
begin
  Result := FRender.Kind;
end;

function TCustomStyledGraphicButton.GetLayout: TButtonLayout;
begin
  Result := FRender.Layout;
end;

function TCustomStyledGraphicButton.ImageMarginsStored: Boolean;
begin
  Result := not FRender.IsDefaultImageMargins;
end;

function TCustomStyledGraphicButton.IsCaptionStored: Boolean;
begin
  if (ActionLink = nil) then
    Result := Caption <> ''
  else
    Result := not TGraphicButtonActionLink(ActionLink).IsCaptionLinked;
end;

function TCustomStyledGraphicButton.IsCheckedStored: Boolean;
begin
  if (ActionLink = nil) then
    Result := FRender.Down
  else
    Result := not TGraphicButtonActionLink(ActionLink).IsCheckedLinked and
      (FRender.Down);
end;

function TCustomStyledGraphicButton.IsEnabledStored: Boolean;
begin
  if (ActionLink = nil) then
    Result := not Enabled
  else
    Result := not TGraphicButtonActionLink(ActionLink).IsEnabledLinked;
end;

function TCustomStyledGraphicButton.IsImageIndexStored: Boolean;
begin
  if (ActionLink = nil) then
    Result := FImageIndex <> -1
  else
    Result := not TGraphicButtonActionLink(ActionLink).IsImageIndexLinked and
      (FImageIndex <> -1);
end;

function TCustomStyledGraphicButton.IsNotificationBadgeStored: Boolean;
begin
  Result := FRender.IsNotificationBadgeStored;
end;

function TCustomStyledGraphicButton.IsCustomDrawType: Boolean;
begin
  Result := FRender.IsCustomDrawType;
end;

function TCustomStyledGraphicButton.IsCustomRadius: Boolean;
begin
  Result := FRender.IsCustomRadius;
end;

function TCustomStyledGraphicButton.IsStoredStyleFamily: Boolean;
begin
  Result := FRender.IsStoredStyleFamily;
end;

function TCustomStyledGraphicButton.IsStoredStyleClass: Boolean;
begin
  Result := FRender.IsStoredStyleClass;
end;

function TCustomStyledGraphicButton.IsStoredStyleAppearance: Boolean;
begin
  Result := FRender.IsStoredStyleAppearance;
end;

function TCustomStyledGraphicButton.IsStoredStyleElements: Boolean;
begin
  Result := FRender.IsStoredStyleElements;
end;

function TCustomStyledGraphicButton.IsStyleDisabledStored: Boolean;
begin
  Result := FRender.IsStyleDisabledStored;
end;

function TCustomStyledGraphicButton.IsStylePressedStored: Boolean;
begin
  Result := FRender.IsStylePressedStored;
end;

function TCustomStyledGraphicButton.IsStyleSelectedStored: Boolean;
begin
  Result := FRender.IsStyleSelectedStored;
end;

function TCustomStyledGraphicButton.IsStyleHotStored: Boolean;
begin
  Result := FRender.IsStyleHotStored;
end;

function TCustomStyledGraphicButton.IsStyleNormalStored: Boolean;
begin
  Result := FRender.IsStyleNormalStored;
end;

function TCustomStyledGraphicButton.GetButtonState: TStyledButtonState;
begin
  //Getting button state
  if not Enabled then
    Result := bsmDisabled
  else if (FRender.State = bsDown) or (FRender.Down) then
    Result := bsmPressed
  else if Focused then
    Result := bsmSelected
  else if FRender.MouseInControl then
    Result := bsmHot
  else
    Result := bsmNormal;
end;

function TCustomStyledGraphicButton.GetFlat: Boolean;
begin
  Result := FRender.Flat;
end;

function TCustomStyledGraphicButton.GetFocused: Boolean;
begin
  Result := False;
end;

function TCustomStyledGraphicButton.GetGlyph: TBitmap;
begin
  Result := FRender.Glyph;
end;

function TCustomStyledGraphicButton.GetGroupIndex: Integer;
begin
  Result := FRender.GroupIndex;
end;

procedure TCustomStyledGraphicButton.SetGlyph(const AValue: TBitmap);
begin
  FRender.Glyph := AValue;
end;

procedure TCustomStyledGraphicButton.SetGroupIndex(const AValue: Integer);
begin
  FRender.GroupIndex := AValue;
end;

procedure TCustomStyledGraphicButton.Paint;
begin
  FRender.DrawButton(Canvas, not Transparent);
end;

class procedure TCustomStyledGraphicButton.RegisterDefaultRenderingStyle(
  const ADrawType: TStyledButtonDrawType; const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass; const AAppearance: TStyledButtonAppearance;
  const AStyleRadius: Integer; const ACursor: TCursor);
begin
  _DefaultStyleDrawType := ADrawType;
  _UseCustomDrawType := True;
  _DefaultFamily := AFamily;
  _DefaultClass := AClass;
  _DefaultAppearance := AAppearance;
  _DefaultStyleRadius := AStyleRadius;
  _DefaultCursor := ACursor;
end;

function TCustomStyledGraphicButton.GetRenderClass: TStyledButtonRenderClass;
begin
  Result := TStyledButtonRender;
end;

function TCustomStyledGraphicButton.GetRescalingButton: Boolean;
begin
  Result := Assigned(FRender) and FRender.RescalingButton;
end;

procedure TCustomStyledGraphicButton.SetRescalingButton(const AValue: Boolean);
begin
  if Assigned(FRender) then
    FRender.RescalingButton := AValue;
end;

function TCustomStyledGraphicButton.GetStyleDrawType: TStyledButtonDrawType;
begin
  Result := FRender.StyleDrawType;
end;

procedure TCustomStyledGraphicButton.SetStyleDrawType(const AValue: TStyledButtonDrawType);
begin
  FRender.StyleDrawType := AValue;
end;

function TCustomStyledGraphicButton.GetImage(out AImageList: TCustomImageList;
  out AImageIndex: Integer): Boolean;
begin
  Result := FRender.GetInternalImage(AImageList, AImageIndex);
end;

function TCustomStyledGraphicButton.GetImageAlignment: TImageAlignment;
begin
  Result := FRender.ImageAlignment;
end;

procedure TCustomStyledGraphicButton.SetImageAlignment(const AValue: TImageAlignment);
begin
  FRender.ImageAlignment := AValue;
end;

function TCustomStyledGraphicButton.GetDisabledImageIndex: TImageIndex;
begin
  Result := FRender.DisabledImageIndex;
end;

procedure TCustomStyledGraphicButton.SetDisabledImageIndex(const AValue: TImageIndex);
begin
  FRender.DisabledImageIndex := AValue;
end;

procedure TCustomStyledGraphicButton.SetImageIndex(const AValue: TImageIndex);
begin
  if AValue <> FImageIndex then
  begin
    FImageIndex := AValue;
    FRender.ImageIndex := AValue;
  end;
end;

{$IFDEF D10_4+}
function TCustomStyledGraphicButton.GetDisabledImageName: TImageName;
begin
  Result := FRender.DisabledImageName;
end;

procedure TCustomStyledGraphicButton.SetDisabledImageName(const AValue: TImageName);
begin
  FRender.DisabledImageName := AValue;
end;

function TCustomStyledGraphicButton.IsImageNameStored: Boolean;
begin
  Result := (ActionLink = nil) or
    not TGraphicButtonActionLink(ActionLink).IsImageNameLinked;
end;

function TCustomStyledGraphicButton.GetImageName: TImageName;
begin
  Result := FImageName;
end;

procedure TCustomStyledGraphicButton.SetImageName(const AValue: TImageName);
begin
  if AValue <> FImageName then
  begin
    FImageName := AValue;
    FRender.ImageName := AValue;
  end;
end;

function TCustomStyledGraphicButton.GetHotImageName: TImageName;
begin
  Result := FRender.HotImageName;
end;

procedure TCustomStyledGraphicButton.SetHotImageName(const AValue: TImageName);
begin
  FRender.HotImageName := AValue;
end;

function TCustomStyledGraphicButton.GetPressedImageName: TImageName;
begin
  Result := FRender.PressedImageName;
end;

procedure TCustomStyledGraphicButton.SetPressedImageName(const AValue: TImageName);
begin
  FRender.PressedImageName := AValue;
end;

function TCustomStyledGraphicButton.GetSelectedImageName: TImageName;
begin
  Result := FRender.SelectedImageName;
end;

procedure TCustomStyledGraphicButton.SetSelectedImageName(const AValue: TImageName);
begin
  FRender.SelectedImageName := AValue;
end;
{$ENDIF}

function TCustomStyledGraphicButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TGraphicButtonActionLink;
end;

function TCustomStyledGraphicButton.GetActiveStyleName: string;
begin
  Result := FRender.ActiveStyleName;
end;

function TCustomStyledGraphicButton.GetAllowAllUp: Boolean;
begin
  Result := FRender.AllowAllUp;
end;

function TCustomStyledGraphicButton.GetAsVCLComponent: Boolean;
begin
  Result := FRender.AsVCLComponent;
end;

function TCustomStyledGraphicButton.GetAutoClick: Boolean;
begin
  Result := FRender.AutoClick;
end;

function TCustomStyledGraphicButton.GetAutoClickDelay: Integer;
begin
  Result := FRender.AutoClickDelay;
end;

function TCustomStyledGraphicButton.GetDisabledImages: TCustomImageList;
begin
  Result := FRender.DisabledImages;
end;

function TCustomStyledGraphicButton.GetDown: Boolean;
begin
  Result := FRender.Down;
end;

procedure TCustomStyledGraphicButton.SetDisabledImages(const AValue: TCustomImageList);
begin
  FRender.DisabledImages := AValue;
end;

procedure TCustomStyledGraphicButton.SetDown(const AValue: Boolean);
begin
  FRender.Down := AValue;
end;

function TCustomStyledGraphicButton.GetDropDownMenu: TPopupMenu;
begin
  Result := FRender.DropDownMenu;
end;

procedure TCustomStyledGraphicButton.SetDropDownMenu(const AValue: TPopupMenu);
begin
  FRender.DropDownMenu := AValue;
end;

procedure TCustomStyledGraphicButton.SetFlat(const AValue: Boolean);
begin
  FRender.Flat := AValue;
end;

function TCustomStyledGraphicButton.GetHotImageIndex: TImageIndex;
begin
  Result := FRender.HotImageIndex;
end;

procedure TCustomStyledGraphicButton.SetHotImageIndex(const AValue: TImageIndex);
begin
  FRender.HotImageIndex := AValue;
end;

function TCustomStyledGraphicButton.GetImages: TCustomImageList;
begin
  Result := FRender.Images;
end;

procedure TCustomStyledGraphicButton.SetImages(const AValue: TCustomImageList);
begin
  FRender.Images := AValue;
end;

procedure TCustomStyledGraphicButton.SetKind(const AValue: TBitBtnKind);
begin
  FRender.Kind := AValue;
end;

function TCustomStyledGraphicButton.GetMargin: Integer;
begin
  Result := FRender.Margin;
end;

procedure TCustomStyledGraphicButton.SetLayout(const AValue: TButtonLayout);
begin
  FRender.Layout := AValue;
end;

function TCustomStyledGraphicButton.GetModalResult: TModalResult;
begin
  Result := FRender.ModalResult;
end;

function TCustomStyledGraphicButton.GetMouseInControl: Boolean;
begin
  Result := FRender.MouseInControl;
end;

function TCustomStyledGraphicButton.GetNotificationBadge: TNotificationBadgeAttributes;
begin
  Result := FRender.NotificationBadge;
end;

function TCustomStyledGraphicButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := FRender.NumGlyphs;
end;

procedure TCustomStyledGraphicButton.SetMargin(const AValue: Integer);
begin
  FRender.Margin := AValue;
end;

procedure TCustomStyledGraphicButton.SetModalResult(const AValue: TModalResult);
begin
  FRender.ModalResult := AValue;
end;

procedure TCustomStyledGraphicButton.SetName(const AValue: TComponentName);
var
  LOldValue: string;
begin
  LOldValue := Caption;
  inherited;
  if LOldValue <> Caption then
    Invalidate;
end;

procedure TCustomStyledGraphicButton.SetNotificationBadge(
  const AValue: TNotificationBadgeAttributes);
begin
  FRender.NotificationBadge := AValue;
end;

function TCustomStyledGraphicButton.GetOnDropDownClick: TNotifyEvent;
begin
  Result := FRender.OnDropDownClick;
end;

procedure TCustomStyledGraphicButton.SetOnDropDownClick(const AValue: TNotifyEvent);
begin
  FRender.OnDropDownClick := AValue;
end;

function TCustomStyledGraphicButton.GetPressedImageIndex: TImageIndex;
begin
  Result := FRender.PressedImageIndex;
end;

procedure TCustomStyledGraphicButton.SetPressedImageIndex(const AValue: TImageIndex);
begin
  FRender.PressedImageIndex := AValue;
end;

function TCustomStyledGraphicButton.GetButtonStyleNormal: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStyleNormal;
end;

procedure TCustomStyledGraphicButton.SetButtonStyleNormal(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStyleNormal := AValue;
end;

procedure TCustomStyledGraphicButton.SetButtonStyle(
  const AStyleFamily: TStyledButtonFamily;
  const AStyleClass: TStyledButtonClass;
  const AStyleAppearance: TStyledButtonAppearance);
begin
  FRender.SetButtonStyle(AStyleFamily, AStyleClass, AStyleAppearance);
end;

procedure TCustomStyledGraphicButton.SetAllowAllUp(const AValue: Boolean);
begin
  FRender.AllowAllUp := AValue;
end;

procedure TCustomStyledGraphicButton.SetAsVCLComponent(const AValue: Boolean);
begin
  FRender.AsVCLComponent := AValue;
end;

procedure TCustomStyledGraphicButton.SetAutoClick(const AValue: Boolean);
begin
  FRender.AutoClick := AValue;
end;

procedure TCustomStyledGraphicButton.SetAutoClickDelay(const AValue: Integer);
begin
  FRender.AutoClickDelay := AValue;
end;

procedure TCustomStyledGraphicButton.SetButtonStyle(const AStyleFamily: TStyledButtonFamily;
  const AModalResult: TModalResult);
begin
  FRender.SetButtonStyle(AStyleFamily, AModalResult);
end;

function TCustomStyledGraphicButton.GetButtonStyleDisabled: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStyleDisabled;
end;

procedure TCustomStyledGraphicButton.SetButtonStyleDisabled(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStyleDisabled := AValue;
end;

function TCustomStyledGraphicButton.GetButtonStylePressed: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStylePressed;
end;

procedure TCustomStyledGraphicButton.SetButtonStylePressed(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStylePressed := AValue;
end;

function TCustomStyledGraphicButton.GetButtonStyleSelected: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStyleSelected;
end;

procedure TCustomStyledGraphicButton.SetButtonStyleSelected(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStyleSelected := AValue;
end;

function TCustomStyledGraphicButton.GetButtonStyleHot: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStyleHot;
end;

procedure TCustomStyledGraphicButton.SetButtonStyleHot(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStyleHot := AValue;
end;

function TCustomStyledGraphicButton.GetImageMargins: TImageMargins;
begin
  Result := FRender.ImageMargins;
end;

procedure TCustomStyledGraphicButton.SetImageMargins(const AValue: TImageMargins);
begin
  FRender.ImageMargins := AValue;
end;

function TCustomStyledGraphicButton.GetStyleRadius: Integer;
begin
  Result := FRender.StyleRadius;
end;

function TCustomStyledGraphicButton.GetStyleRoundedCorners: TRoundedCorners;
begin
  Result := FRender.StyleRoundedCorners;
end;

procedure TCustomStyledGraphicButton.SetStyleRadius(const AValue: Integer);
begin
  FRender.StyleRadius := AValue;
end;

procedure TCustomStyledGraphicButton.SetStyleRoundedCorners(
  const AValue: TRoundedCorners);
begin
  FRender.StyleRoundedCorners := AValue;
end;

function TCustomStyledGraphicButton.GetSelectedImageIndex: TImageIndex;
begin
  Result := FRender.SelectedImageIndex;
end;

function TCustomStyledGraphicButton.GetShowCaption: Boolean;
begin
  Result := FRender.ShowCaption;
end;

function TCustomStyledGraphicButton.GetSpacing: Integer;
begin
  Result := FRender.Spacing;
end;

function TCustomStyledGraphicButton.GetSplitButtonWidth: Integer;
begin
  Result := FRender.GetSplitButtonWidth;
end;

procedure TCustomStyledGraphicButton.SetSelectedImageIndex(const AValue: TImageIndex);
begin
  FRender.SelectedImageIndex := AValue;
end;

procedure TCustomStyledGraphicButton.SetShowCaption(const AValue: Boolean);
begin
  FRender.ShowCaption := AValue;
end;

procedure TCustomStyledGraphicButton.SetSpacing(const AValue: Integer);
begin
  FRender.Spacing := AValue;
end;

function TCustomStyledGraphicButton.GetStyle: TCustomButton.TButtonStyle;
begin
  Result := FRender.Style;
end;

procedure TCustomStyledGraphicButton.SetStyle(const AValue: TCustomButton.TButtonStyle);
begin
  FRender.Style := AValue;
end;

function TCustomStyledGraphicButton.GetStyleAppearance: TStyledButtonAppearance;
begin
  Result := FRender.StyleAppearance;
end;

procedure TCustomStyledGraphicButton.SetStyleAppearance(const AValue: TStyledButtonAppearance);
begin
  FRender.StyleAppearance := AValue;
end;

function TCustomStyledGraphicButton.GetStyleApplied: Boolean;
begin
  Result := FRender.StyleApplied;
end;

procedure TCustomStyledGraphicButton.SetStyleApplied(const AValue: Boolean);
begin
  FRender.StyleApplied := AValue;
end;

function TCustomStyledGraphicButton.GetStyleClass: TStyledButtonClass;
begin
  Result := FRender.StyleClass;
end;

procedure TCustomStyledGraphicButton.SetStyleClass(const AValue: TStyledButtonClass);
begin
  FRender.StyleClass := AValue;
end;

function TCustomStyledGraphicButton.GetStyleFamily: TStyledButtonFamily;
begin
  Result := FRender.StyleFamily;
end;

procedure TCustomStyledGraphicButton.SetStyleFamily(const AValue: TStyledButtonFamily);
begin
  FRender.StyleFamily := AValue;
end;

{$IFDEF D10_4+}
procedure TCustomStyledGraphicButton.SetStyleName(const AValue: string);
begin
  if (AValue <> '') and (StyleFamily <> DEFAULT_CLASSIC_FAMILY) then
    StyleFamily := DEFAULT_CLASSIC_FAMILY;
  inherited;
  if (AValue <> '') then
    StyleClass := AValue;  
end;
{$ENDIF}

procedure TCustomStyledGraphicButton.SetText(const AValue: TCaption);
begin
  FRender.Caption := AValue;
end;

procedure TCustomStyledGraphicButton.SetTransparent(const AValue: Boolean);
begin
  FRender.Transparent := AValue;
end;

function TCustomStyledGraphicButton.GetWordWrap: Boolean;
begin
  Result := FRender.WordWrap;
end;

function TCustomStyledGraphicButton.HasCustomGlyph: Boolean;
var
  Link: TGraphicButtonActionLink;
begin
  Link := TGraphicButtonActionLink(ActionLink);
  Result := not ((Link <> nil) and Link.IsImageIndexLinked and
    Link.IsGlyphLinked(ImageIndex));
end;

procedure TCustomStyledGraphicButton.SetWordWrap(const AValue: Boolean);
begin
  FRender.WordWrap := AValue;
end;

procedure TCustomStyledGraphicButton.ShowDropDownMenu;
begin
  FRender.ShowDropDownMenu;
end;

procedure TCustomStyledGraphicButton.Loaded;
begin
  inherited;
  FRender.Loaded;
end;

procedure TCustomStyledGraphicButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FRender.MouseDown(Button, Shift, X, Y);
  if Enabled then
    inherited;
end;

procedure TCustomStyledGraphicButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FRender.MouseMove(Shift, X, Y);
end;

procedure TCustomStyledGraphicButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then
  begin
    FRender.MouseUp(Button, Shift, X, Y);
    inherited;
  end;
end;

procedure TCustomStyledGraphicButton.ControlClick(Sender: TObject);
begin
  inherited Click;
end;

procedure TCustomStyledGraphicButton.ControlFont(var AValue: TFont);
begin
  AValue := Self.Font;
end;

procedure TCustomStyledGraphicButton.SetParentFont(const AValue: Boolean);
begin
  Self.ParentFont := AValue;
end;

function TCustomStyledGraphicButton.GetParentFont: Boolean;
begin
  Result := Self.ParentFont;
end;

function TCustomStyledGraphicButton.GetCaption: TCaption;
begin
  Result := inherited Caption;
end;

function TCustomStyledGraphicButton.GetCaptionToDraw: TCaption;
begin
  Result := inherited Caption;
end;

function TCustomStyledGraphicButton.GetCaptionAlignment: TAlignment;
begin
  Result := FRender.CaptionAlignment;
end;

function TCustomStyledGraphicButton.GetCommandLinkHint: string;
begin
  Result := FRender.CommandLinkHint;
end;

function TCustomStyledGraphicButton.GetCursor: TCursor;
begin
  Result := inherited Cursor;
end;

procedure TCustomStyledGraphicButton.SetCaption(const AValue: TCaption);
begin
  inherited Caption := AValue;
end;

procedure TCustomStyledGraphicButton.SetCaptionAlignment(
  const AValue: TAlignment);
begin
  FRender.CaptionAlignment := AValue;
end;

procedure TCustomStyledGraphicButton.SetCommandLinkHint(const AValue: string);
begin
  FRender.CommandLinkHint := AValue;
end;

procedure TCustomStyledGraphicButton.SetCursor(const AValue: TCursor);
begin
  if AValue <> Cursor then
  begin
    inherited Cursor := AValue;
  end;
end;

procedure TCustomStyledGraphicButton.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if AOperation = opRemove then
  begin
    if Assigned(FRender) then
    begin
      if AComponent = DropDownMenu then
        DropDownMenu := nil;
      if AComponent = Images then
        Images := nil;
      if AComponent = DisabledImages then
        DisabledImages := nil;
    end;
  end;
end;

function TCustomStyledGraphicButton.GetTag: Integer;
begin
  Result := FRender.Tag;
end;

procedure TCustomStyledGraphicButton.SetTag(const AValue: Integer);
begin
  inherited Tag := AValue;
  FRender.Tag := AValue;
end;

{ TGraphicButtonActionLink }

procedure TGraphicButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TControl;
end;

function TGraphicButtonActionLink.AssignedClientRender: Boolean;
begin
  Result := ClientRender <> nil;
end;

function TGraphicButtonActionLink.ClientRender: TStyledButtonRender;
begin
  if FClient is TCustomStyledGraphicButton then
    Result := TCustomStyledGraphicButton(FClient).FRender
  else if FClient is TCustomStyledButton then
    Result := TCustomStyledButton(FClient).FRender
  else
    Result := nil;
end;

function TGraphicButtonActionLink.IsCheckedLinked: Boolean;
begin
  if ClientRender <> nil then
  begin
    Result := inherited IsCheckedLinked and AssignedClientRender and
      (ClientRender.Down = TCustomAction(Action).Checked);
  end
  else
    Result := inherited IsCheckedLinked;
end;

function TGraphicButtonActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and
    (FClient.Enabled =
      TCustomAction(Action).Enabled);
end;

function TGraphicButtonActionLink.IsGlyphLinked(Index: TImageIndex): Boolean;
var
  LBitmap: TBitmap;
  Images: TCustomImageList;
  LGlyph: TBitmap;
  LRender: TStyledButtonRender;
begin
  Result := False;
  if FClient is TCustomStyledGraphicButton then
  begin
    LGlyph := TCustomStyledGraphicButton(FClient).Glyph;
    LRender := TCustomStyledGraphicButton(FClient).Render;
  end
  else if FClient is TCustomStyledButton then
  begin
    LGlyph := TCustomStyledButton(FClient).Glyph;
    LRender := TCustomStyledButton(FClient).Render;
  end
  else
  begin
    LGlyph := nil;
    LRender := nil;
  end;
  if Assigned(LGlyph) then
  begin
    Images := TCustomAction(Action).ActionList.Images;
    Result := (Images <> nil) and (LGlyph <> nil) and
      (LGlyph.Width = Images.Width) and (LGlyph.Height = Images.Height);
    if Result then
    begin
      LBitmap := TBitmap.Create;
      try
        LRender.InternalCopyImage(LBitmap, Images, Index);
        Result := LBitmap.Equals(LGlyph);
      finally
        LBitmap.Free;
      end;
    end;
  end;
end;

function TGraphicButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Assert(Assigned(FClient));
  if FClient is TCustomStyledButton then
    Result := inherited IsImageIndexLinked and
      (TCustomStyledButton(FClient).ImageIndex =
        TCustomAction(Action).ImageIndex)
  else if FClient is TCustomStyledGraphicButton then
    Result := inherited IsImageIndexLinked and
      (TCustomStyledGraphicButton(FClient).ImageIndex =
        TCustomAction(Action).ImageIndex)
  else
    Result := False;
end;

{$IFDEF D10_4+}
function TGraphicButtonActionLink.IsImageNameLinked: Boolean;
begin
  Result := inherited IsImageNameLinked and
    (TCustomStyledGraphicButton(FClient).ImageName =
      TCustomAction(Action).ImageName);
end;
{$ENDIF}

procedure TGraphicButtonActionLink.SetChecked(Value: Boolean);
begin
  inherited;
  if IsCheckedLinked and AssignedClientRender then
    ClientRender.Down := Value;
end;

procedure TGraphicButtonActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then
    FClient.Enabled := Value;
end;

procedure TGraphicButtonActionLink.SetGroupIndex(Value: Integer);
begin
  inherited;
  if IsGroupIndexLinked and AssignedClientRender then
    ClientRender.GroupIndex := Value;
end;

procedure TGraphicButtonActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
  begin
    if (FClient is TCustomStyledButton) then
      TCustomStyledButton(FClient).ImageIndex := Value
    else if (FClient is TCustomStyledGraphicButton) then
      TCustomStyledGraphicButton(FClient).ImageIndex := Value;
  end;
end;

{ TStyledSpeedButton }

constructor TStyledSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRender.SetText('');
  FRender.Spacing := 4;
  FRender.FUseButtonLayout := True;
  FRender.Transparent := True;
  SetBounds(0, 0, 23, 22); //As default VCL SpeedButton
  ParentFont := True;
end;

{ TCustomStyledButton }

procedure TCustomStyledButton.AssignStyleTo(ADestRender: TStyledButtonRender);
begin
  FRender.AssignStyleTo(ADestRender);
end;

procedure TCustomStyledButton.AssignStyleTo(ADest: TCustomStyledButton);
begin
  FRender.AssignStyleTo(ADest.Render);
end;

function TCustomStyledButton.AssignAttributes(
  const AEnabled: Boolean = True;
  const AImageList: TCustomImageList = nil;
  {$IFDEF D10_4+}const AImageName: string = '';{$ENDIF}
  const AImageIndex: Integer = -1;
  const AImageAlignment: TImageAlignment = iaLeft;
  const AAction: TCustomAction = nil;
  const AOnClick: TNotifyEvent = nil;
  const AName: string = ''): TCustomStyledButton;
begin
  Result := FRender.AssignAttributes(AEnabled,
    AImageList,
    {$IFDEF D10_4+}AImageName,{$ENDIF}
    AImageIndex,
    AImageAlignment,
    AAction,
    AOnClick,
    AName) as TCustomStyledButton;
end;

procedure TCustomStyledButton.AssignTo(ADest: TPersistent);
var
  LDest: TCustomStyledButton;
begin
  inherited AssignTo(ADest);
  if ADest is TCustomStyledButton then
  begin
  if ADest is TCustomStyledButton then
  begin
    LDest := TCustomStyledButton(ADest);
    FRender.AssignStyleTo(LDest.Render);
    LDest.Cursor := Self.Cursor;
    LDest.Hint := Self.Hint;
    LDest.Visible := Self.Visible;
    LDest.Caption := Self.Caption;
    LDest.ModalResult := Self.ModalResult;
    LDest.Tag := Self.Tag;
    LDest.Enabled := Self.Enabled;
    LDest.TabStop := Self.TabStop;
  end;
  end;
end;

procedure TCustomStyledButton.BeginUpdate;
begin
  FRender.BeginUpdate;
end;

procedure TCustomStyledButton.EndUpdate;
begin
  FRender.EndUpdate;
end;

{$IFDEF HiDPISupport}
procedure TCustomStyledButton.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  if isDpiChange then
  begin
    RescalingButton := True;
    try
      if Assigned(FRender) then
        FRender.ChangeScale(M, D, isDpiChange);
      inherited;
    finally
      RescalingButton := False;
    end;
  end
  else
    inherited;
  end;
{$ENDIF}

procedure TCustomStyledButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FRender.CMEnabledChanged(Message);
end;

procedure TCustomStyledButton.CMEnter(var Message: TCMEnter);
begin
  inherited;
  FRender.CMEnter(Message);
end;

procedure TCustomStyledButton.CMMouseEnter(var Message: TNotifyEvent);
begin
  inherited;
  FRender.CMMouseEnter(Message);
end;

procedure TCustomStyledButton.CMMouseLeave(var Message: TNotifyEvent);
begin
  inherited;
  FRender.CMMouseLeave(Message);
end;

procedure TCustomStyledButton.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  FRender.CMStyleChanged(Message);
end;

constructor TCustomStyledButton.CreateStyled(AOwner: TComponent;
  const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  ParentColor := False;
  FImageIndex := -1;
  FRender := GetRenderClass.CreateStyled(Self,
    ControlClick, ControlFont, GetCaption, SetCaption,
      GetParentFont, SetParentFont,
      AFamily, AClass, AAppearance,
      _DefaultStyleDrawType, _DefaultCursor, _UseCustomDrawType);
  TabStop := True;
end;

constructor TCustomStyledButton.CreateStyled(AOwner: TComponent;
  const AFamily: TStyledButtonFamily; const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance; const ADrawType: TStyledButtonDrawType;
  const ACursor: TCursor; const AUseCustomDrawType: Boolean);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  ParentColor := False;
  FImageIndex := -1;
  FRender := GetRenderClass.CreateStyled(Self,
    ControlClick, ControlFont, GetCaption, SetCaption,
      GetParentFont, SetParentFont,
      AFamily, AClass, AAppearance,
      ADrawType, ACursor, AUseCustomDrawType);
  TabStop := True;
end;

procedure TCustomStyledButton.CreateWnd;
begin
  inherited CreateWnd;
  FRender.Active := Default;
end;

constructor TCustomStyledButton.Create(AOwner: TComponent);
begin
  CreateStyled(AOwner,
    _DefaultFamily,
    _DefaultClass,
    _DefaultAppearance);
end;

procedure TCustomStyledButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited;
  FRender.ActionChange(Sender, CheckDefaults);
end;

destructor TCustomStyledButton.Destroy;
begin
  FreeAndNil(FRender);
  FreeAndNil(FPaintBuffer);
  inherited Destroy;
end;

function TCustomStyledButton.CalcImageRect(var ATextRect: TRect; const AImageWidth,
  AImageHeight: Integer): TRect;
begin
  Result := FRender.CalcImageRect(ATextRect, AImageWidth, AImageHeight);
end;

function TCustomStyledButton.CanDropDownMenu: boolean;
begin
  Result := FRender.CanDropDownMenu;
end;

procedure TCustomStyledButton.DoDropDownMenu;
begin
  FRender.DoDropDownMenu;
end;

function TCustomStyledButton.GetText: TCaption;
begin
  Result := FRender.GetText;
end;

function TCustomStyledButton.GetKind: TBitBtnKind;
begin
  Result := FRender.Kind;
end;

function TCustomStyledButton.GetLayout: TButtonLayout;
begin
  Result := FRender.Layout;
end;

function TCustomStyledButton.ImageMarginsStored: Boolean;
begin
  Result := not FRender.IsDefaultImageMargins;
end;

function TCustomStyledButton.IsCaptionAlignmentStored: Boolean;
begin
  Result := FRender.IsCaptionAlignmentStored;
end;

function TCustomStyledButton.IsCaptionStored: Boolean;
begin
  if (ActionLink = nil) then
    Result := Caption <> ''
  else
    Result := not TGraphicButtonActionLink(ActionLink).IsCaptionLinked;
end;

function TCustomStyledButton.IsCheckedStored: Boolean;
begin
  if (ActionLink = nil) then
    Result := FRender.Down
  else
    Result := not TGraphicButtonActionLink(ActionLink).IsCheckedLinked and
      (FRender.Down);
end;

function TCustomStyledButton.IsEnabledStored: Boolean;
begin
  if (ActionLink = nil) then
    Result := not Enabled
  else
    Result := not TGraphicButtonActionLink(ActionLink).IsEnabledLinked;
end;

function TCustomStyledButton.IsImageIndexStored: Boolean;
begin
  if (ActionLink = nil) then
    Result := ImageIndex <> -1
  else
    Result := not TGraphicButtonActionLink(ActionLink).IsImageIndexLinked;
end;

function TCustomStyledButton.IsNotificationBadgeStored: Boolean;
begin
  Result := FRender.IsNotificationBadgeStored;
end;

function TCustomStyledButton.IsCustomDrawType: Boolean;
begin
  Result := FRender.IsCustomDrawType;
end;

function TCustomStyledButton.IsCustomRadius: Boolean;
begin
  Result := FRender.IsCustomRadius;
end;

function TCustomStyledButton.IsStoredStyleFamily: Boolean;
begin
  Result := FRender.IsStoredStyleFamily;
end;

function TCustomStyledButton.IsStoredStyleClass: Boolean;
begin
  Result := FRender.IsStoredStyleClass;
end;

function TCustomStyledButton.IsStoredStyleAppearance: Boolean;
begin
  Result := FRender.IsStoredStyleAppearance;
end;

function TCustomStyledButton.IsStoredStyleElements: Boolean;
begin
  Result := FRender.IsStoredStyleElements;
end;

function TCustomStyledButton.IsStyleDisabledStored: Boolean;
begin
  Result := FRender.IsStyleDisabledStored;
end;

function TCustomStyledButton.IsStylePressedStored: Boolean;
begin
  Result := FRender.IsStylePressedStored;
end;

function TCustomStyledButton.IsStyleSelectedStored: Boolean;
begin
  Result := FRender.IsStyleSelectedStored;
end;

function TCustomStyledButton.IsStyleHotStored: Boolean;
begin
  Result := FRender.IsStyleHotStored;
end;

function TCustomStyledButton.IsStyleNormalStored: Boolean;
begin
  Result := FRender.IsStyleNormalStored;
end;

function TCustomStyledButton.GetButtonState: TStyledButtonState;
begin
  //Getting button state
  if not Enabled then
    Result := bsmDisabled
  else if FRender.State = bsDown then
    Result := bsmPressed
  else if Focused then
    Result := bsmSelected
  else if FRender.MouseInControl then
    Result := bsmHot
  else
    Result := bsmNormal;
end;

function TCustomStyledButton.GetRenderClass: TStyledButtonRenderClass;
begin
  Result := TStyledButtonRender;
end;

function TCustomStyledButton.GetRescalingButton: Boolean;
begin
  Result := Assigned(FRender) and FRender.RescalingButton;
end;

procedure TCustomStyledButton.SetRescalingButton(const AValue: Boolean);
begin
  if Assigned(FRender) then
    FRender.RescalingButton := AValue;
end;

function TCustomStyledButton.GetStyleDrawType: TStyledButtonDrawType;
begin
  Result := FRender.StyleDrawType;
end;

procedure TCustomStyledButton.SetStyleDrawType(const AValue: TStyledButtonDrawType);
begin
  FRender.StyleDrawType := AValue;
end;

function TCustomStyledButton.GetImage(out AImageList: TCustomImageList;
  out AImageIndex: Integer): Boolean;
begin
  Result := FRender.GetInternalImage(AImageList, AImageIndex);
end;

function TCustomStyledButton.GetImageAlignment: TImageAlignment;
begin
  Result := FRender.ImageAlignment;
end;

procedure TCustomStyledButton.SetImageAlignment(const AValue: TImageAlignment);
begin
  FRender.ImageAlignment := AValue;
end;

function TCustomStyledButton.GetDefault: Boolean;
begin
  Result := FRender.Default;
end;

procedure TCustomStyledButton.SetDefault(const AValue: Boolean);
var
  Form: TCustomForm;
begin
  if FRender.Default <> AValue then
  begin
    FRender.Default := AValue;
    if HandleAllocated then
    begin
      Form := GetParentForm(Self);
      if (Form <> nil) and (Form.ActiveControl <> nil) then
        Form.Perform(CM_FOCUSCHANGED, 0, LPARAM(Form.ActiveControl));
    end;
    if (csLoading in ComponentState) then
      FRender.Active := FRender.Default;
  end;
end;

function TCustomStyledButton.GetCancel: Boolean;
begin
  Result := FRender.Cancel;
end;

procedure TCustomStyledButton.SetCancel(const AValue: Boolean);
begin
  if FRender.Cancel <> AValue then
    FRender.Cancel := AValue;
end;

function TCustomStyledButton.GetDisabledImageIndex: TImageIndex;
begin
  Result := FRender.DisabledImageIndex;
end;

procedure TCustomStyledButton.SetDisabledImageIndex(const AValue: TImageIndex);
begin
  FRender.DisabledImageIndex := AValue;
end;

procedure TCustomStyledButton.SetImageIndex(const AValue: TImageIndex);
begin
  if AValue <> FImageIndex then
  begin
    FImageIndex := AValue;
    FRender.ImageIndex := AValue;
  end;
end;

{$IFDEF D10_4+}
function TCustomStyledButton.GetDisabledImageName: TImageName;
begin
  Result := FRender.DisabledImageName;
end;

procedure TCustomStyledButton.SetDisabledImageName(const AValue: TImageName);
begin
  FRender.DisabledImageName := AValue;
end;

function TCustomStyledButton.IsImageNameStored: Boolean;
begin
  Result := (ActionLink = nil) or
    not TGraphicButtonActionLink(ActionLink).IsImageNameLinked;
end;

function TCustomStyledButton.GetImageName: TImageName;
begin
  Result := FRender.ImageName;
end;

procedure TCustomStyledButton.SetImageName(const AValue: TImageName);
begin
  if AValue <> FImageName then
  begin
    FImageName := AValue;
    FRender.ImageName := AValue;
  end;
end;

function TCustomStyledButton.GetHotImageName: TImageName;
begin
  Result := FRender.HotImageName;
end;

procedure TCustomStyledButton.SetHotImageName(const AValue: TImageName);
begin
  FRender.HotImageName := AValue;
end;

function TCustomStyledButton.GetStylusHotImageName: TImageName;
begin
  Result := FRender.StylusHotImageName;
end;

procedure TCustomStyledButton.SetStylusHotImageName(const AValue: TImageName);
begin
  FRender.StylusHotImageName := AValue;
end;

function TCustomStyledButton.GetPressedImageName: TImageName;
begin
  Result := FRender.PressedImageName;
end;

procedure TCustomStyledButton.SetPressedImageName(const AValue: TImageName);
begin
  FRender.PressedImageName := AValue;
end;

function TCustomStyledButton.GetSelectedImageName: TImageName;
begin
  Result := FRender.SelectedImageName;
end;

procedure TCustomStyledButton.SetSelectedImageName(const AValue: TImageName);
begin
  FRender.SelectedImageName := AValue;
end;
{$ENDIF}

function TCustomStyledButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TGraphicButtonActionLink;
end;

function TCustomStyledButton.GetActiveStyleName: string;
begin
  Result := FRender.ActiveStyleName;
end;

function TCustomStyledButton.GetAllowAllUp: Boolean;
begin
  Result := FRender.AllowAllUp;
end;

function TCustomStyledButton.GetAsVCLComponent: Boolean;
begin
  Result := FRender.AsVCLComponent;
end;

function TCustomStyledButton.GetAutoClick: Boolean;
begin
  Result := FRender.AutoClick;
end;

function TCustomStyledButton.GetAutoClickDelay: Integer;
begin
  Result := FRender.AutoClickDelay;
end;

function TCustomStyledButton.GetDisabledImages: TCustomImageList;
begin
  Result := FRender.DisabledImages;
end;

function TCustomStyledButton.GetDown: Boolean;
begin
  Result := FRender.Down;
end;

procedure TCustomStyledButton.SetDisabledImages(const AValue: TCustomImageList);
begin
  FRender.DisabledImages := AValue;
end;

procedure TCustomStyledButton.SetDown(const AValue: Boolean);
begin
  FRender.Down := AValue;
end;

function TCustomStyledButton.GetDropDownMenu: TPopupMenu;
begin
  Result := FRender.DropDownMenu;
end;

function TCustomStyledButton.GetElevationRequired: Boolean;
begin
  Result := FRender.ElevationRequired;
end;

function TCustomStyledButton.GetFlat: Boolean;
begin
  Result := FRender.Flat;
end;

function TCustomStyledButton.GetGlyph: TBitmap;
begin
  Result := FRender.Glyph;
end;

function TCustomStyledButton.GetGroupIndex: Integer;
begin
  Result := FRender.GroupIndex;
end;

procedure TCustomStyledButton.SetGlyph(const AValue: TBitmap);
begin
  FRender.Glyph := AValue;
end;

procedure TCustomStyledButton.SetGroupIndex(const AValue: Integer);
begin
  FRender.GroupIndex := AValue;
end;

procedure TCustomStyledButton.SetDropDownMenu(const AValue: TPopupMenu);
begin
  FRender.DropDownMenu := AValue;
end;

procedure TCustomStyledButton.SetElevationRequired(const AValue: Boolean);
begin
  FRender.ElevationRequired := AValue;
end;

procedure TCustomStyledButton.SetFlat(const AValue: Boolean);
begin
  FRender.Flat := AValue;
end;

function TCustomStyledButton.GetHotImageIndex: TImageIndex;
begin
  Result := FRender.HotImageIndex;
end;

procedure TCustomStyledButton.SetHotImageIndex(const AValue: TImageIndex);
begin
  FRender.HotImageIndex := AValue;
end;

function TCustomStyledButton.GetStylusHotImageIndex: TImageIndex;
begin
  Result := FRender.StylusHotImageIndex;
end;

procedure TCustomStyledButton.SetStylusHotImageIndex(const AValue: TImageIndex);
begin
  FRender.StylusHotImageIndex := AValue;
end;

function TCustomStyledButton.GetImages: TCustomImageList;
begin
  Result := FRender.Images;
end;

procedure TCustomStyledButton.SetImages(const AValue: TCustomImageList);
begin
  FRender.Images := AValue;
end;

procedure TCustomStyledButton.SetKind(const AValue: TBitBtnKind);
begin
  FRender.Kind := AValue;
end;

function TCustomStyledButton.GetMargin: Integer;
begin
  Result := FRender.Margin;
end;

procedure TCustomStyledButton.SetLayout(const AValue: TButtonLayout);
begin
  FRender.Layout := AValue;
end;

function TCustomStyledButton.GetModalResult: TModalResult;
begin
  Result := FRender.ModalResult;
end;

function TCustomStyledButton.GetMouseInControl: Boolean;
begin
  Result := FRender.MouseInControl;
end;

function TCustomStyledButton.GetNotificationBadge: TNotificationBadgeAttributes;
begin
  Result := FRender.NotificationBadge;
end;

function TCustomStyledButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := FRender.NumGlyphs;
end;

procedure TCustomStyledButton.SetNumGlyphs(const AValue: TNumGlyphs);
begin
  FRender.NumGlyphs := AValue;
end;

procedure TCustomStyledButton.SetMargin(const AValue: Integer);
begin
  FRender.Margin := AValue;
end;

procedure TCustomStyledButton.SetModalResult(const AValue: TModalResult);
begin
  FRender.ModalResult := AValue;
end;

procedure TCustomStyledButton.SetName(const AValue: TComponentName);
var
  LOldValue: string;
begin
  LOldValue := Caption;
  inherited;
  if LOldValue <> Caption then
    Invalidate;
end;

procedure TCustomStyledButton.SetNotificationBadge(
  const AValue: TNotificationBadgeAttributes);
begin
  FRender.NotificationBadge := AValue;
end;

function TCustomStyledButton.GetOnDropDownClick: TNotifyEvent;
begin
  Result := FRender.OnDropDownClick;
end;

procedure TCustomStyledButton.SetOnDropDownClick(const AValue: TNotifyEvent);
begin
  FRender.OnDropDownClick := AValue;
end;

function TCustomStyledButton.GetPressedImageIndex: TImageIndex;
begin
  Result := FRender.PressedImageIndex;
end;

procedure TCustomStyledButton.SetPressedImageIndex(const AValue: TImageIndex);
begin
  FRender.PressedImageIndex := AValue;
end;

function TCustomStyledButton.GetButtonStyleNormal: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStyleNormal;
end;

procedure TCustomStyledButton.SetButtonStyleNormal(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStyleNormal := AValue;
end;

procedure TCustomStyledButton.SetButtonStyle(
  const AStyleFamily: TStyledButtonFamily;
  const AStyleClass: TStyledButtonClass;
  const AStyleAppearance: TStyledButtonAppearance);
begin
  FRender.SetButtonStyle(AStyleFamily, AStyleClass, AStyleAppearance);
end;

procedure TCustomStyledButton.SetAllowAllUp(const AValue: Boolean);
begin
  FRender.AllowAllUp := AValue;
end;

procedure TCustomStyledButton.SetAsVCLComponent(const AValue: Boolean);
begin
  FRender.AsVCLComponent := AValue;
end;

procedure TCustomStyledButton.SetAutoClick(const AValue: Boolean);
begin
  FRender.AutoClick := AValue;
end;

procedure TCustomStyledButton.SetAutoClickDelay(const AValue: Integer);
begin
  FRender.AutoClickDelay := AValue;
end;

procedure TCustomStyledButton.SetButtonStyle(const AStyleFamily: TStyledButtonFamily;
  const AModalResult: TModalResult);
begin
  FRender.SetButtonStyle(AStyleFamily, AModalResult);
end;

function TCustomStyledButton.GetButtonStyleDisabled: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStyleDisabled;
end;

procedure TCustomStyledButton.SetButtonStyleDisabled(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStyleDisabled := AValue;
end;

function TCustomStyledButton.GetButtonStylePressed: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStylePressed;
end;

procedure TCustomStyledButton.SetButtonStylePressed(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStylePressed := AValue;
end;

function TCustomStyledButton.GetButtonStyleSelected: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStyleSelected;
end;

procedure TCustomStyledButton.SetButtonStyleSelected(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStyleSelected := AValue;
end;

function TCustomStyledButton.GetButtonStyleHot: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStyleHot;
end;

procedure TCustomStyledButton.SetButtonStyleHot(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStyleHot := AValue;
end;

function TCustomStyledButton.GetImageMargins: TImageMargins;
begin
  Result := FRender.ImageMargins;
end;

procedure TCustomStyledButton.SetImageMargins(const AValue: TImageMargins);
begin
  FRender.ImageMargins := AValue;
end;

function TCustomStyledButton.GetStyleRadius: Integer;
begin
  Result := FRender.StyleRadius;
end;

function TCustomStyledButton.GetStyleRoundedCorners: TRoundedCorners;
begin
  Result := FRender.StyleRoundedCorners;
end;

procedure TCustomStyledButton.SetStyleRadius(const AValue: Integer);
begin
  FRender.StyleRadius := AValue;
end;

procedure TCustomStyledButton.SetStyleRoundedCorners(
  const AValue: TRoundedCorners);
begin
  FRender.StyleRoundedCorners := AValue;
end;

function TCustomStyledButton.GetSelectedImageIndex: TImageIndex;
begin
  Result := FRender.SelectedImageIndex;
end;

function TCustomStyledButton.GetShowCaption: Boolean;
begin
  Result := FRender.ShowCaption;
end;

function TCustomStyledButton.GetSpacing: Integer;
begin
  Result := FRender.Spacing;
end;

function TCustomStyledButton.GetSplitButtonWidth: Integer;
begin
  Result := FRender.GetSplitButtonWidth;
end;

procedure TCustomStyledButton.SetSelectedImageIndex(const AValue: TImageIndex);
begin
  FRender.SelectedImageIndex := AValue;
end;

procedure TCustomStyledButton.SetShowCaption(const AValue: Boolean);
begin
  FRender.ShowCaption := AValue;
end;

procedure TCustomStyledButton.SetSpacing(const AValue: Integer);
begin
  FRender.Spacing := AValue;
end;

function TCustomStyledButton.GetStyle: TCustomButton.TButtonStyle;
begin
  Result := FRender.Style;
end;

procedure TCustomStyledButton.SetStyle(const AValue: TCustomButton.TButtonStyle);
begin
  FRender.Style := AValue;
end;

function TCustomStyledButton.GetStyleAppearance: TStyledButtonAppearance;
begin
  Result := FRender.StyleAppearance;
end;

procedure TCustomStyledButton.SetStyleAppearance(const AValue: TStyledButtonAppearance);
begin
  FRender.StyleAppearance := AValue;
end;

function TCustomStyledButton.GetStyleApplied: Boolean;
begin
  Result := FRender.StyleApplied;
end;

procedure TCustomStyledButton.SetStyleApplied(const AValue: Boolean);
begin
  FRender.StyleApplied := AValue;
end;

function TCustomStyledButton.GetStyleClass: TStyledButtonClass;
begin
  Result := FRender.StyleClass;
end;

procedure TCustomStyledButton.SetStyleClass(const AValue: TStyledButtonClass);
begin
  FRender.StyleClass := AValue;
end;

function TCustomStyledButton.GetStyleFamily: TStyledButtonFamily;
begin
  Result := FRender.StyleFamily;
end;

procedure TCustomStyledButton.SetStyleFamily(const AValue: TStyledButtonFamily);
begin
  FRender.StyleFamily := AValue;
end;

{$IFDEF D10_4+}
procedure TCustomStyledButton.SetStyleName(const AValue: string);
begin
  if (AValue <> '') and (StyleFamily <> DEFAULT_CLASSIC_FAMILY) then
    StyleFamily := DEFAULT_CLASSIC_FAMILY;
  inherited;
  if (AValue <> '') then
    StyleClass := AValue;  
end;
{$ENDIF}

procedure TCustomStyledButton.SetText(const AValue: TCaption);
begin
  FRender.Caption := AValue;
end;

function TCustomStyledButton.GetWordWrap: Boolean;
begin
  Result := FRender.WordWrap;
end;

function TCustomStyledButton.HasCustomGlyph: Boolean;
var
  Link: TGraphicButtonActionLink;
begin
  Link := TGraphicButtonActionLink(ActionLink);
  Result := not ((Link <> nil) and Link.IsImageIndexLinked and
    Link.IsGlyphLinked(ImageIndex));
end;

procedure TCustomStyledButton.SetWordWrap(const AValue: Boolean);
begin
  FRender.WordWrap := AValue;
end;

procedure TCustomStyledButton.ShowDropDownMenu;
begin
  FRender.ShowDropDownMenu;
end;

procedure TCustomStyledButton.Loaded;
begin
  inherited;
  FRender.Loaded;
end;

procedure TCustomStyledButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FRender.MouseDown(Button, Shift, X, Y);
  if Enabled then
    inherited;
end;

procedure TCustomStyledButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FRender.MouseMove(Shift, X, Y);
end;

procedure TCustomStyledButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then
  begin
    FRender.MouseUp(Button, Shift, X, Y);
    inherited;
  end;
end;

procedure TCustomStyledButton.ControlClick(Sender: TObject);
begin
  inherited Click;
end;

procedure TCustomStyledButton.ControlFont(var AValue: TFont);
begin
  AValue := Self.Font;
end;

procedure TCustomStyledButton.SetParentFont(const AValue: Boolean);
begin
  Self.ParentFont := AValue;
end;

function TCustomStyledButton.GetParentFont: Boolean;
begin
  Result := Self.ParentFont;
end;

function TCustomStyledButton.GetCaption: TCaption;
begin
  Result := inherited Caption;
end;

function TCustomStyledButton.GetCaptionToDraw: TCaption;
begin
  Result := inherited Caption;
end;

function TCustomStyledButton.GetCaptionAlignment: TAlignment;
begin
  Result := FRender.CaptionAlignment;
end;

function TCustomStyledButton.GetCommandLinkHint: string;
begin
  Result := FRender.CommandLinkHint;
end;

function TCustomStyledButton.GetCursor: TCursor;
begin
  Result := inherited Cursor;
end;

procedure TCustomStyledButton.SetCaption(const AValue: TCaption);
begin
  inherited Caption := AValue;
end;

procedure TCustomStyledButton.SetCaptionAlignment(const AValue: TAlignment);
begin
  FRender.CaptionAlignment := AValue;
end;

procedure TCustomStyledButton.SetCommandLinkHint(const AValue: string);
begin
  FRender.CommandLinkHint := AValue;
end;

procedure TCustomStyledButton.SetCursor(const AValue: TCursor);
begin
  if AValue <> Cursor then
  begin
    inherited Cursor := AValue;
  end;
end;

procedure TCustomStyledButton.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if AOperation = opRemove then
  begin
    if Assigned(FRender) then
    begin
      if AComponent = DropDownMenu then
        DropDownMenu := nil;
      if AComponent = Images then
        Images := nil;
      if AComponent = DisabledImages then
        DisabledImages := nil;
    end;
  end;
end;

class procedure TCustomStyledButton.RegisterDefaultRenderingStyle(
  const ADrawType: TStyledButtonDrawType; const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass; const AAppearance: TStyledButtonAppearance;
  const AStyleRadius: Integer; const ACursor: TCursor);
begin
  _DefaultStyleDrawType := ADrawType;
  _UseCustomDrawType := True;
  _DefaultFamily := AFamily;
  _DefaultClass := AClass;
  _DefaultAppearance := AAppearance;
  _DefaultStyleRadius := AStyleRadius;
  _DefaultCursor := ACursor;
end;

procedure TCustomStyledButton.ReleasePaintBuffer;
begin
  if FPaintBufferUsers = 0 then
    FreeAndNil(FPaintBuffer);
end;

function TCustomStyledButton.GetTag: Integer;
begin
  Result := FRender.Tag;
end;

procedure TCustomStyledButton.SetTag(const AValue: Integer);
begin
  inherited Tag := AValue;
  FRender.Tag := AValue;
end;

procedure TCustomStyledButton.Click;
begin
  FRender.Click(False);
end;

procedure TCustomStyledButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TCustomStyledButton.CMDialogKey(var Message: TCMDialogKey);
begin
  with Message do
    if  (((CharCode = VK_RETURN) and FRender.Active) or
      ((CharCode = VK_ESCAPE) and Cancel)) and
      (KeyDataToShiftState(Message.KeyData) = []) and CanFocus then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

procedure TCustomStyledButton.WMEraseBkGnd(var Message: TWmEraseBkgnd);
begin
  inherited;
  //if (TMessage(Message).wParam = WPARAM(TMessage(Message).lParam)) then
  //  FRender.EraseBackground(Message.DC);
  Message.Result := 1;
end;

procedure TCustomStyledButton.CNKeyDown(var Message: TWMKeyDown);
begin
  with Message do
  begin
    Result := 1;
    if not (csDesigning in ComponentState) then
    begin
      if (CharCode = VK_DOWN) and (CanDropDownMenu) then
        DoDropDownMenu
      else
        inherited;
    end;
    Result := 0;
  end;
end;

procedure TCustomStyledButton.WMKeyDown(var Message: TMessage);
begin
  inherited;
  FRender.WMKeyDown(Message);
end;

procedure TCustomStyledButton.WMKeyUp(var Message: TMessage);
begin
  inherited;
  FRender.WMKeyUp(Message);
end;

procedure TCustomStyledButton.CMFocusChanged(var Message: TCMFocusChanged);
begin
  with Message do
    if Sender is TCustomStyledButton then
      FRender.Active := Sender = Self
    else
      FRender.Active := Default;
  inherited;
end;

procedure TCustomStyledButton.WMSetFocus(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomStyledButton.WMKillFocus(var Message: TMessage);
begin
  Invalidate;
  inherited;
end;

procedure TCustomStyledButton.WMPaint(var Message: TMessage);
var
  DC: HDC;
  LCanvas: TCanvas;
  PS: TPaintStruct;
begin
  //if FOverridePaint then
  begin
    DC := HDC(Message.WParam);
    LCanvas := TCanvas.Create;
    try
      if DC <> 0 then
        LCanvas.Handle := DC
      else
        LCanvas.Handle := BeginPaint(Self.Handle, PS);
      if FDoubleBuffered and (DC = 0) then
      begin
        if FPaintBuffer = nil then
          FPaintBuffer := TBitmap.Create;
        Inc(FPaintBufferUsers);
        try
          FPaintBuffer.SetSize(Self.Width, Self.Height);
          FRender.DrawButton(FPaintBuffer.Canvas, True);
          // paint other controls
          PaintControls(FPaintBuffer.Canvas.Handle, nil);
          LCanvas.Draw(0, 0, FPaintBuffer);
        finally
          Dec(FPaintBufferUsers);
          ReleasePaintBuffer;
        end;
      end
      else
      begin
        if not DoubleBuffered and (FPaintBuffer <> nil) then
          ReleasePaintBuffer;
        FRender.DrawButton(LCanvas, True);
        // paint other controls
        PaintControls(LCanvas.Handle, nil);
      end;
      if DC = 0 then
        EndPaint(Self.Handle, PS);
    finally
      LCanvas.Handle := 0;
      LCanvas.Free;
    end;
  end;
  Message.Result := 1;
end;

{ TStyledBitBtn }

constructor TStyledBitBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRender.FUseButtonLayout := True;
  FRender.Spacing := 4;
end;

function TStyledBitBtn.IsCaptionStored: Boolean;
begin
  Result := AnsiCompareStr(Caption, FRender.BitBtnCaptions(FRender.Kind)) <> 0;
end;

initialization
  TCustomStyledGraphicButton._DefaultStyleDrawType := DEFAULT_STYLEDRAWTYPE;
  TCustomStyledGraphicButton._DefaultFamily := DEFAULT_CLASSIC_FAMILY;
  TCustomStyledGraphicButton._DefaultClass := DEFAULT_WINDOWS_CLASS;
  TCustomStyledGraphicButton._DefaultAppearance := DEFAULT_APPEARANCE;
  TCustomStyledGraphicButton._DefaultStyleRadius := DEFAULT_RADIUS;
  TCustomStyledGraphicButton._DefaultCursor := DEFAULT_CURSOR;

  TCustomStyledButton._DefaultStyleDrawType := DEFAULT_STYLEDRAWTYPE;
  TCustomStyledButton._DefaultFamily := DEFAULT_CLASSIC_FAMILY;
  TCustomStyledButton._DefaultClass := DEFAULT_WINDOWS_CLASS;
  TCustomStyledButton._DefaultAppearance := DEFAULT_APPEARANCE;
  TCustomStyledButton._DefaultStyleRadius := DEFAULT_RADIUS;
  TCustomStyledButton._DefaultCursor := DEFAULT_CURSOR;

end.
