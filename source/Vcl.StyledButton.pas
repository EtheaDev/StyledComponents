{******************************************************************************}
{                                                                              }
{       TStyledGraphicButton: a Button Component based on TGraphicControl      }
{       TStyledButton: a Button Component based on TCustomControl              }
{                                                                              }
{       Copyright (c) 2022-2023 (Ethea S.r.l.)                                 }
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
unit Vcl.StyledButton;

interface

{$INCLUDE StyledComponents.inc}

uses
  Vcl.ImgList
  , System.Math
  , System.UITypes
  , Winapi.Windows
  , Winapi.CommCtrl
  , Winapi.Messages
  , Vcl.Graphics
  , Vcl.buttons
  , System.SysUtils
  , System.Classes
  , Vcl.StdCtrls
  , Vcl.Themes
  , Vcl.Controls
  , Vcl.ActnList
  , Vcl.Menus
  , Vcl.ButtonStylesAttributes
  ;

const
  StyledButtonsVersion = '2.1.0';
  DEFAULT_BTN_WIDTH = 75;
  DEFAULT_BTN_HEIGHT = 25;
  DEFAULT_IMAGE_HMARGIN = 8;
  DEFAULT_IMAGE_VMARGIN = 4;

resourcestring
  ERROR_SETTING_BUTTON_STYLE = 'Error setting Button Style: %s/%s/%s not available';
  ERROR_CANNOT_USE_RENDER = 'Error: cannot use TStyledButtonRender in this context';

type
  EStyledButtonError = Exception;

  TStyledButtonState = (bsmNormal, bsmPressed, bsmSelected, bsmHot, bsmDisabled);
  TStyledButtonStyle = (bsPushButton, bsSplitButton);

  TStyledButtonRender = class;
  TStyledButtonRenderClass = class of TStyledButtonRender;

  TGraphicButtonActionLink = class(TControlActionLink)
  strict private
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
  public
    function IsCheckedLinked: Boolean; override;
    function IsGlyphLinked(Index: TImageIndex): Boolean; virtual;
  end;

  TControlFont = procedure (var AFont: TFont) of Object;

  TSetCaption = procedure (const ACaption: TCaption) of Object;
  TGetCaption = function : TCaption of Object;

  TSetParentFont = procedure (const AParentFont: Boolean) of Object;
  TGetParentFont = function: Boolean of Object;

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
    FModalResult: TModalResult;
    FMouseInControl: Boolean;
    FState: TButtonState;
    FImageMargins: TImageMargins;

    FStyleRadius: Integer;
    FStyleDrawType: TStyledButtonDrawType;
    FStyleFamily: TStyledButtonFamily;
    FStyleClass: TStyledButtonClass;
    FStyleAppearance: TStyledButtonAppearance;

    FCustomDrawType: Boolean;
    FStyleApplied: Boolean;

    FDisabledImages: TCustomImageList;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;

    FDisabledImageIndex: TImageIndex;
    FHotImageIndex: TImageIndex;
    FPressedImageIndex: TImageIndex;
    FSelectedImageIndex: TImageIndex;
    {$IFDEF D10_4+}
    FDisabledImageName: TImageName;
    FHotImageName: TImageName;
    FPressedImageName: TImageName;
    FSelectedImageName: TImageName;
    {$ENDIF}

    FImageAlignment: TImageAlignment;
    FTag: Integer;
    FWordWrap: Boolean;
    FActive: Boolean;
    FDefault: Boolean;
    FCancel: Boolean;
    FKind: TBitBtnKind;
    FStyle: TStyledButtonStyle;
    FDropDownMenu: TPopupMenu;
    FDropDownRect: TRect;
    FOnDropDownClick: TNotifyEvent;
    FMouseOverDropDown: Boolean;
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
    FFlat: Boolean;
    procedure SetImageMargins(const AValue: TImageMargins);
    procedure SetStyleRadius(const AValue: Integer);
    procedure SetStyleFamily(const AValue: TStyledButtonFamily);
    procedure SetStyleClass(const AValue: TStyledButtonClass);
    procedure SetStyleAppearance(const AValue: TStyledButtonAppearance);
    function ApplyButtonStyle: Boolean;

    procedure SetDisabledImages(const AValue: TCustomImageList);
    procedure SetImages(const AValue: TCustomImageList);

    procedure SetDisabledImageIndex(const AValue: TImageIndex);
    procedure SetHotImageIndex(const AValue: TImageIndex);
    function GetImageIndex: TImageIndex;
    procedure SetImageIndex(const AValue: TImageIndex);
    procedure SetPressedImageIndex(const AValue: TImageIndex);
    procedure SetSelectedImageIndex(const AValue: TImageIndex);

    {$IFDEF D10_4+}
    procedure UpdateImageIndex(Name: TImageName; var Index: TImageIndex);
    procedure UpdateImageName(Index: TImageIndex; var Name: TImageName);
    procedure SetDisabledImageName(const AValue: TImageName);
    procedure SetHotImageName(const AValue: TImageName);
    function GetImageName: TImageName;
    procedure SetImageName(const AValue: TImageName);
    procedure SetPressedImageName(const AValue: TImageName);
    procedure SetSelectedImageName(const AValue: TImageName);
    {$ENDIF}

    function GetAttributes(const AMode: TStyledButtonState): TStyledButtonAttributes;
    procedure ImageMarginsChange(Sender: TObject);
    procedure SetImageAlignment(const AValue: TImageAlignment);
    procedure DrawBackgroundAndBorder(const ACanvas: TCanvas;
      const AStyleAttribute: TStyledButtonAttributes);
    procedure DrawText(const ACanvas: TCanvas;
      const AText: string; var ARect: TRect; AFlags: Cardinal);
    function GetDrawingStyle(const ACanvas: TCanvas): TStyledButtonAttributes;
    procedure SetStyleDrawType(const AValue: TStyledButtonDrawType);
    procedure ImageListChange(Sender: TObject);
    procedure SetText(const AValue: TCaption);

    procedure SetButtonStylePressed(const AValue: TStyledButtonAttributes);
    procedure SetButtonStyleSelected(const AValue: TStyledButtonAttributes);
    procedure SetButtonStyleHot(const AValue: TStyledButtonAttributes);
    procedure SetButtonStyleNormal(const AValue: TStyledButtonAttributes);
    procedure SetButtonStyleDisabled(const AValue: TStyledButtonAttributes);

    procedure UpdateControlStyle;
    procedure SetWordWrap(const AValue: Boolean);
    procedure SetStyleApplied(const AValue: Boolean);
    function GetKind: TBitBtnKind;
    procedure SetKind(const Value: TBitBtnKind);
    function BitBtnCaptions(Kind: TBitBtnKind): string;
    function UpdateStyleUsingModalResult: boolean;
    procedure SetDropDownMenu(const Value: TPopupMenu);
    procedure SetStyle(const Value: TStyledButtonStyle);
    function GetActiveStyleName: string;
    function AsVCLStyle: Boolean;

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
    procedure Invalidate;
    function GetHint: string;
    function GetButtonState: TStyledButtonState;
    function GetHandle: HWND;
    procedure CalcDefaultImageMargins(const AValue: TImageAlignment);
    procedure SetGlyph(const AValue: TBitmap);
    function GetGlyph: TBitmap;
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(const AValue: TNumGlyphs);
    procedure SetFlat(const AValue: Boolean);
  private
    procedure SetState(const AValue: TButtonState);
    function GetMouseInControl: Boolean;
  protected
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
    function CalcImageRect(var ATextRect: TRect;
      const AImageWidth, AImageHeight: Integer): TRect;
    procedure InternalCopyImage(Image: TBitmap; ImageList: TCustomImageList; Index: Integer);
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
    procedure UpdateStyleElements;
    procedure EraseBackground(const ACanvas: TCanvas);
    procedure DrawButton(const ACanvas: TCanvas);
    procedure DrawImage(const ACanvas: TCanvas;
      var ATextRect: TRect);
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
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean);
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
      const AName: string = ''): TControl;

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
      const AAppearance: TStyledButtonAppearance);
    constructor Create(AOwner: TControl;
      const AOnClick: TNotifyEvent;
      const AControlFont: TControlFont;
      const AGetCaption: TGetCaption;
      const ASetCaption: TSetCaption;
      const AGetParentFont: TGetParentFont;
      const ASetParentFont: TSetParentFont);
    destructor Destroy; override;
    function IsDefaultAppearance: Boolean;
    property Active: Boolean read FActive write FActive;
    property Focused: Boolean read GetFocused;
    property ButtonState: TStyledButtonState read GetButtonState;
    property StyleApplied: Boolean read FStyleApplied write SetStyleApplied;
    property Caption: TCaption read GetText write SetText;
    property Default: Boolean read FDefault write FDefault;
    property Cancel: Boolean read FCancel write FCancel;
    property ActiveStyleName: string read GetActiveStyleName;
    property ImageAlignment: TImageAlignment read FImageAlignment write SetImageAlignment;
    property DisabledImageIndex: TImageIndex read FDisabledImageIndex write SetDisabledImageIndex;
    property DisabledImages: TCustomImageList read FDisabledImages write SetDisabledImages;
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    property Flat: Boolean read FFlat write SetFlat;
    property SplitButtonWidth: Integer read GetSplitButtonWidth;
    property HotImageIndex: TImageIndex read FHotImageIndex write SetHotImageIndex;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex;
    property Kind: TBitBtnKind read GetKind write SetKind default bkCustom;
    property OwnerControl: TControl read FOwnerControl;
    property PressedImageIndex: TImageIndex read FPressedImageIndex write SetPressedImageIndex;
    property SelectedImageIndex: TImageIndex read FSelectedImageIndex write SetSelectedImageIndex;

    {$IFDEF D10_4+}
    property DisabledImageName: TImageName read FDisabledImageName write SetDisabledImageName;
    property HotImageName: TImageName read FHotImageName write SetHotImageName;
    property ImageName: TImageName read GetImageName write SetImageName;
    property PressedImageName: TImageName read FPressedImageName write SetPressedImageName;
    property SelectedImageName: TImageName read FSelectedImageName write SetSelectedImageName;
    {$ENDIF}
    property ImageMargins: TImageMargins read FImageMargins write SetImageMargins;
    property ModalResult: TModalResult read FModalResult write SetModalResult;
    property RescalingButton: Boolean read GetRescalingButton write SetRescalingButton;
    //Style as TButton
    property Style: TStyledButtonStyle read FStyle write SetStyle;

    //StyledComponents Attributes
    property StyleRadius: Integer read FStyleRadius write SetStyleRadius;
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
  end;

  TStyledGraphicButton = class;
  TStyledButton = class;

  TStyledGraphicButton = class(TGraphicControl)
  private
    FRender: TStyledButtonRender;
    FImageIndex: TImageIndex;
    {$IFDEF D10_4+}
    FImageName: TImageName;
    {$ENDIF}
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
    function GetStyle: TStyledButtonStyle;
    procedure SetStyle(const AValue: TStyledButtonStyle);
    //function GetActiveStyleName: string;
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
  protected
    procedure SetCursor(const AValue: TCursor); virtual;
    function GetCaption: TCaption; virtual;
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
    procedure UpdateStyleElements; override;
    function GetRenderClass: TStyledButtonRenderClass; virtual;
  public
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
    procedure AssignStyleTo(ADest: TStyledGraphicButton); overload;
    procedure AssignTo(ADest: TPersistent); override;
    function AssignAttributes(
      const AEnabled: Boolean = True;
      const AImageList: TCustomImageList = nil;
      {$IFDEF D10_4+}const AImageName: string = '';{$ENDIF}
      const AImageIndex: Integer = -1;
      const AImageAlignment: TImageAlignment = iaLeft;
      const AAction: TCustomAction = nil;
      const AOnClick: TNotifyEvent = nil;
      const AName: string = ''): TStyledGraphicButton;
    procedure Click; override;
    procedure DoDropDownMenu;
    constructor CreateStyled(AOwner: TComponent;
      const AFamily: TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance); virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Focused: Boolean read GetFocused;
    property ButtonState: TStyledButtonState read GetButtonState;
    property MouseInControl: Boolean read GetMouseInControl;
    property Render: TStyledButtonRender read FRender;
    property StyleApplied: Boolean read GetStyleApplied write SetStyleApplied;
    property RescalingButton: Boolean read GetRescalingButton write SetRescalingButton;
  published
    //property ActiveStyleName: string read GetActiveStyleName write FActiveStyleName stored false;
    property Action;
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled stored IsEnabledStored;
    property Font;
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
    property ParentFont default true;
    property ParentShowHint;
    property ShowHint;
    {$IFDEF D10_4+}
    property StyleName;
    {$ENDIF}
    property StyleElements stored IsStoredStyleElements;
    property Touch;
    property Visible;
    property Caption: TCaption read GetText write SetText stored IsCaptionStored;
    property Cursor: TCursor read GetCursor write SetCursor default crHandPoint;
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
    //Style as TButton
    property Style: TStyledButtonStyle read GetStyle write SetStyle default bsPushButton;
    //StyledComponents Attributes
    property StyleRadius: Integer read GetStyleRadius write SetStyleRadius stored IsCustomRadius default DEFAULT_RADIUS;
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
    property OnDropDownClick: TNotifyEvent read GetOnDropDownClick write SetOnDropDownClick;
  end;

  TStyledButton = class(TCustomControl)
  private
    FRender: TStyledButtonRender;
    FImageIndex: TImageIndex;
    FHandled: Boolean;
    {$IFDEF D10_4+}
    FImageName: TImageName;
    {$ENDIF}
    //function StyleServicesEnabled: Boolean;
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
    function GetStyle: TStyledButtonStyle;
    procedure SetStyle(const AValue: TStyledButtonStyle);
    function CanDropDownMenu: boolean;
    //function GetActiveStyleName: string;
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
  protected
    procedure SetCursor(const AValue: TCursor); virtual;
    function CalcImageRect(var ATextRect: TRect;
      const AImageWidth, AImageHeight: Integer): TRect; virtual;
    function GetCaption: TCaption; virtual;
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
    //procedure Paint; overload; override;
    {$IFDEF HiDPISupport}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ENDIF}
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure UpdateStyleElements; override;
    //for StyledButton
    procedure CreateWnd; override;
    function GetRenderClass: TStyledButtonRenderClass; virtual;
  public
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
    procedure AssignStyleTo(ADest: TStyledButton); overload;
    procedure AssignTo(ADest: TPersistent); override;
    function AssignAttributes(
      const AEnabled: Boolean = True;
      const AImageList: TCustomImageList = nil;
      {$IFDEF D10_4+}const AImageName: string = '';{$ENDIF}
      const AImageIndex: Integer = -1;
      const AImageAlignment: TImageAlignment = iaLeft;
      const AAction: TCustomAction = nil;
      const AOnClick: TNotifyEvent = nil;
      const AName: string = ''): TStyledButton;
    procedure DoDropDownMenu;
    constructor CreateStyled(AOwner: TComponent;
      const AFamily: TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance); virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ButtonState: TStyledButtonState read GetButtonState;
    property MouseInControl: Boolean read GetMouseInControl;
    property StyleApplied: Boolean read GetStyleApplied write SetStyleApplied;
    //For StyledButton
    procedure Click; override;
    property Render: TStyledButtonRender read FRender;
    property RescalingButton: Boolean read GetRescalingButton write SetRescalingButton;
  published
    //property ActiveStyleName: string read GetActiveStyleName write FActiveStyleName stored false;
    property Action;
    property Align;
    property Anchors;
    property Constraints;
    property DoubleBuffered default True;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled stored IsEnabledStored;
    property Font;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
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
    property ParentFont default true;
    property ParentShowHint;
    property ShowHint;
    {$IFDEF D10_4+}
    property StyleName;
    {$ENDIF}
    property StyleElements stored IsStoredStyleElements;
    property TabStop default True;
    property Touch;
    property Visible;
    property Caption: TCaption read GetText write SetText stored IsCaptionStored;
    property Cursor: TCursor read GetCursor write SetCursor default crHandPoint;
    property Default: Boolean read GetDefault write SetDefault default False;
    property Cancel: Boolean read GetCancel write SetCancel default False;
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
    //Style as TButton
    property Style: TStyledButtonStyle read GetStyle write SetStyle default bsPushButton;
    //StyledComponents Attributes
    property StyleRadius: Integer read GetStyleRadius write SetStyleRadius stored IsCustomRadius default DEFAULT_RADIUS;
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
    property OnDropDownClick: TNotifyEvent read GetOnDropDownClick write SetOnDropDownClick;

    //Property for StyledButton
    property TabOrder;
    property OnKeyDown;
    property OnKeyUp;
  end;

//Global function to create a StyledButton
function CreateAndPosStyledButton(const AOwner: TComponent;
  const AParent: TWinControl;
  const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  const ACaption: TCaption;
  const ARectPosition: TRect): TStyledButton;

implementation

uses
  System.Types
  , System.RTLConsts
  , Vcl.Forms
  , Vcl.StandardButtonStyles
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
  {$IFDEF D10_4+}
  DefaultBitBtnGlyphSize = 15;
  {$ELSE}
  DefaultBitBtnGlyphSize = 14;
  {$ENDIF}
  BitBtnModalResults: array[TBitBtnKind] of TModalResult = (
    0, mrOk, mrCancel, 0, mrYes, mrNo, 0, mrAbort, mrRetry, mrIgnore,
    mrAll);

function CreateAndPosStyledButton(const AOwner: TComponent;
  const AParent: TWinControl;
  const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  const ACaption: TCaption;
  const ARectPosition: TRect): TStyledButton;
begin
  Result := TStyledButton.CreateStyled(AOwner, AFamily, AClass, AAppearance);
  Result.Parent := AParent;
  Result.Caption := ACaption;
  Result.SetBounds(ARectPosition.Left, ARectPosition.Top, ARectPosition.Right, ARectPosition.Bottom);
end;

{ TStyledButtonRender }

procedure TStyledButtonRender.AssignStyleTo(ADest: TStyledButtonRender);
begin
  if not ParentFont then
    ADest.Font.Assign(Self.Font);
  ADest.FFlat := FFlat;
  ADest.FStyleRadius := Self.FStyleRadius;
  ADest.FButtonStyleNormal.Assign(Self.FButtonStyleNormal);
  ADest.FButtonStylePressed.Assign(Self.FButtonStylePressed);
  ADest.FButtonStyleSelected.Assign(Self.FButtonStyleSelected);
  ADest.FButtonStyleHot.Assign(Self.FButtonStyleHot);
  ADest.FButtonStyleDisabled.Assign(Self.FButtonStyleDisabled);
  ADest.SetButtonStyles(Self.FStyleFamily,
    Self.FStyleClass, Self.FStyleAppearance);
  ADest.FStyleDrawType := Self.FStyleDrawType;
  ADest.FCustomDrawType := Self.FCustomDrawType;
  if Assigned(FImages) then
  begin
    ADest.FImageMargins.Assign(FImageMargins);
    ADest.FImageAlignment := Self.FImageAlignment;
    ADest.Images := Images;
    ADest.DisabledImageIndex := Self.DisabledImageIndex;
    ADest.ImageIndex := Self.ImageIndex;
    ADest.HotImageIndex := Self.HotImageIndex;
    ADest.SelectedImageIndex := Self.SelectedImageIndex;
    ADest.PressedImageIndex := Self.PressedImageIndex;
    {$IFDEF D10_4+}
    ADest.DisabledImageName := Self.DisabledImageName;
    ADest.ImageName := Self.ImageName;
    ADest.HotImageName := Self.HotImageName;
    ADest.SelectedImageName := Self.SelectedImageName;
    ADest.PressedImageName := Self.PressedImageName;
    {$ENDIF}
  end;
  if Assigned(FDisabledImages) then
  begin
    ADest.DisabledImages := DisabledImages;
    ADest.DisabledImageIndex := Self.DisabledImageIndex;
    {$IFDEF D10_4+}
    ADest.DisabledImageName := Self.DisabledImageName;
    {$ENDIF}
  end;
  if Assigned(FGlyph) then
  begin
    ADest.FTransparentColor := FTransparentColor;
    ADest.NumGlyphs := FNumGlyphs;
    ADest.Glyph := FGlyph;
  end;
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
    FState := bsDisabled
  else
    FState := bsUp;
  if FMouseInControl then
  begin
    FMouseInControl := False;
    Invalidate;
  end;
end;

procedure TStyledButtonRender.CMEnter(var Message: TCMEnter);
begin
  if not(Enabled) or (csDesigning in ComponentState) then
    Exit;
  FMouseInControl := false;
end;

procedure TStyledButtonRender.CMMouseEnter(var Message: TNotifyEvent);
begin
  if not(Enabled) or (csDesigning in ComponentState) then
    Exit;
  FMouseInControl := true;
  Invalidate;
end;

procedure TStyledButtonRender.CMMouseLeave(var Message: TNotifyEvent);
begin
  if not(Enabled) or (csDesigning in ComponentState) then
    Exit;
  State := bsUp;
  FMouseInControl := false;
  Invalidate;
end;

procedure TStyledButtonRender.UpdateControlStyle;
begin
  UpdateStyleElements;
end;

function TStyledButtonRender.UpdateCount: Integer;
begin
  Result := FUpdateCount;
end;

procedure TStyledButtonRender.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  UpdateControlStyle;
  Invalidate;
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
    //State := bsUp; do not "force" state changing
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
  const AAppearance: TStyledButtonAppearance);
begin
  Assert(Assigned(AOwner));
  inherited Create;
  FTransparentColor := clOlive;
  FNumGlyphs := 1;
  FFlat := False;
  //Owner Control "link"
  FOwnerControl := AOwner;
  if (FOwnerControl is TStyledButton) then
    TStyledButton(FOwnerControl).FRender := Self
  else if (FOwnerControl is TStyledGraphicButton) then
    TStyledGraphicButton(FOwnerControl).FRender := Self
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
  FImageAlignment := iaLeft;
  ImageIndex := -1;
  FPressedImageIndex := -1;
  FSelectedImageIndex := -1;

  FStyle := bsPushButton;
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
  FOwnerControl.ControlStyle := [csCaptureMouse, csClickEvents,
    csSetCaption, csDoubleClicks];
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FImageMargins := TImageMargins.Create;
  FImageMargins.Left := DEFAULT_IMAGE_HMARGIN;
  FImageMargins.OnChange := ImageMarginsChange;
  FImageAlignment := iaLeft;
  FCustomDrawType := False;
  FOwnerControl.Cursor := crHandPoint;
  ParentFont := true;
  FOwnerControl.Width := DEFAULT_BTN_WIDTH;
  FOwnerControl.Height := DEFAULT_BTN_HEIGHT;
  FMouseInControl := false;
  FStyleDrawType := btRounded;
  FStyleRadius := DEFAULT_RADIUS;
  FStyleFamily := AFamily;
  FStyleAppearance := AAppearance;
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
    DEFAULT_APPEARANCE);
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
begin
  Result := StyleFamilyCheckAttributes(FStyleFamily,
    FStyleClass, FStyleAppearance, LButtonFamily);
  if Result or (csDesigning in ComponentState) then
  begin
    StyleFamilyUpdateAttributes(
      FStyleFamily,
      FStyleClass,
      FstyleAppearance,
      FButtonStyleNormal,
      FButtonStylePressed,
      FButtonStyleSelected,
      FButtonStyleHot,
      FButtonStyleDisabled);

    if not FCustomDrawType then
      FStyleDrawType := FButtonStyleNormal.DrawType;
  end;
  if Result then
    Invalidate;
end;

destructor TStyledButtonRender.Destroy;
begin
  Images := nil;
  FreeAndNil(FGlyph);
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FImageMargins);
  FreeAndNil(FButtonStyleNormal);
  FreeAndNil(FButtonStylePressed);
  FreeAndNil(FButtonStyleSelected);
  FreeAndNil(FButtonStyleHot);
  FreeAndNil(FButtonStyleDisabled);
  inherited Destroy;
end;

function TStyledButtonRender.CanDropDownMenu: boolean;
begin
  Result := (FStyle = bsSplitButton) and
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
  if (FOwnerControl is TStyledButton) then
  begin
    if TStyledButton(FOwnerControl).HandleAllocated then
      Result := TStyledButton(FOwnerControl).Handle
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

function TStyledButtonRender.GetImage(out AImageList: TCustomImageList;
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
        if FHotImageIndex <> -1 then
        AImageIndex := FHotImageIndex
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
  Result := (FStyleFamily = DEFAULT_CLASSIC_FAMILY) and
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

function TStyledButtonRender.IsStoredStyleClass: Boolean;
var
  LClass: TStyledButtonClass;
  LAppearance: TStyledButtonAppearance;
  LButtonFamily: TButtonFamily;
  LModalResultClass: TStyledButtonClass;
begin
  StyleFamilyCheckAttributes(FStyleFamily, LClass, LAppearance, LButtonFamily);

  if AsVCLStyle then
  begin
    Result := (FStyleClass <> GetActiveStyleName)
      and not SameText(FStyleClass, 'Windows');
  end
  else
  begin
    if FModalResult <> mrNone then
    begin
      LButtonFamily.StyledAttributes.GetStyleByModalResult(FModalResult,
        LModalResultClass, LAppearance);
      Result := FStyleClass <> LModalResultClass;
    end
    else
      Result := FStyleClass <> LClass;
  end;
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

function TStyledButtonRender.IsStyleDisabledStored: Boolean;
begin
  Result := FButtonStyleDisabled.IsChanged;
end;

function TStyledButtonRender.IsStylePressedStored: Boolean;
begin
  Result := FButtonStylePressed.IsChanged;
end;

function TStyledButtonRender.IsStyleSelectedStored: Boolean;
begin
  Result := FButtonStyleSelected.IsChanged;
end;

function TStyledButtonRender.IsStyleHotStored: Boolean;
begin
  Result := FButtonStyleHot.IsChanged;
end;

function TStyledButtonRender.IsStyleNormalStored: Boolean;
begin
  Result := FButtonStyleNormal.IsChanged;
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

procedure TStyledButtonRender.DrawText(const ACanvas: TCanvas;
  const AText: string; var ARect: TRect; AFlags: Cardinal);
var
  TextFormat: TTextFormatFlags;
  R: TRect;

  function CanvasDrawText(ACanvas: TCanvas; const AText: string; var Bounds: TRect; Flag: Cardinal): Integer;
  begin
    SetBkMode(ACanvas.Handle, TRANSPARENT);
    Result := Winapi.Windows.DrawText(ACanvas.Handle, PChar(AText),
      Length(AText), Bounds, Flag)
  end;

begin
  TextFormat := TTextFormatFlags(AFlags);
  //Drawing Caption
  R := ARect;
  CanvasDrawText(ACanvas, AText, R, AFlags or DT_CALCRECT);
  OffsetRect(R, (ARect.Width - R.Width) div 2, (ARect.Height - R.Height) div 2);
  CanvasDrawText(ACanvas, AText, R, AFlags);
end;

procedure TStyledButtonRender.DrawBackgroundAndBorder(
  const ACanvas: TCanvas;
  const AStyleAttribute: TStyledButtonAttributes);
var
  DrawRect, SplitButtonRect: TRect;
begin
  //Erase Background
  EraseBackground(ACanvas);

  //Don't draw button face for Flat Buttons
  if FFlat and (FState in [bsUp, bsDisabled]) and not (FMouseInControl) then
    Exit;

  DrawRect := FOwnerControl.ClientRect;

  //Draw Button Shape
  CanvasDrawshape(ACanvas, DrawRect, FStyleDrawType, FStyleRadius);

  if FDropDownRect.Width > 0 then
  begin
    SplitButtonRect.Left := DrawRect.Width - FDropDownRect.Width;
    //Draw Bar and Triangle
    CanvasDrawBarAndTriangle(ACanvas, FDropDownRect,{$IFDEF D10_3+}FOwnerControl.ScaleFactor{$ELSE}1{$ENDIF},
      ACanvas.Pen.Color, AStyleAttribute.FontColor);
  end;
end;

function TStyledButtonRender.CalcImageRect(var ATextRect: TRect;
  const AImageWidth, AImageHeight: Integer): TRect;
var
  IW, IH, IX, IY: Integer;
begin
  //Calc Image Rect
  IH := AImageHeight;
  IW := AImageWidth;
  if (IH > 0) and (IW > 0) then
  begin
    IX := ATextRect.Left + 2;
    IY := ATextRect.Top + (ATextRect.Height - IH) div 2;
    case FImageAlignment of
      iaCenter:
        begin
          IX := ATextRect.CenterPoint.X - IW div 2;
        end;
      iaLeft:
        begin
          IX := ATextRect.Left + 2;
          Inc(IX, ImageMargins.Left);
          Inc(IY, ImageMargins.Top);
          Dec(IY, ImageMargins.Bottom);
          Inc(ATextRect.Left, IX + IW + ImageMargins.Right);
        end;
      iaRight:
        begin
          IX := ATextRect.Right - IW - 2;
          Dec(IX, ImageMargins.Right);
          Dec(IX, ImageMargins.Left);
          Inc(IY, ImageMargins.Top);
          Dec(IY, ImageMargins.Bottom);
          ATextRect.Right := IX;
        end;
      iaTop:
        begin
          IX := ATextRect.Left + (ATextRect.Width - IW) div 2;
          Inc(IX, ImageMargins.Left);
          Dec(IX, ImageMargins.Right);
          IY := ATextRect.Top + 2;
          Inc(IY, ImageMargins.Top);
          Inc(ATextRect.Top, IY + IH + ImageMargins.Bottom);
        end;
      iaBottom:
        begin
          IX := ATextRect.Left + (ATextRect.Width - IW) div 2;
          Inc(IX, ImageMargins.Left);
          Dec(IX, ImageMargins.Right);
          IY := ATextRect.Bottom - IH - 2;
          Dec(IY, ImageMargins.Bottom);
          Dec(IY, ImageMargins.Top);
          ATextRect.Bottom := IY;
        end;
    end;
  end
  else
  begin
    IX := 0;
    IY := 0;
  end;
  Result.Left := IX;
  Result.Top := IY;
  Result.Width := IW;
  Result.Height := IH;
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
  //Images.CheckIndexAndName(FStylusHotImageIndex, FStylusHotImageName);
  if FDisabledImages <> nil then
    FDisabledImages.CheckIndexAndName(FDisabledImageIndex, FDisabledImageName)
  else
    Images.CheckIndexAndName(FDisabledImageIndex, FDisabledImageName);
end;
{$ENDIF}

procedure TStyledButtonRender.EraseBackground(const ACanvas: TCanvas);
var
  LOwnerControl: TWinControl;
begin
  if FOwnerControl is TWinControl then
    LOwnerControl := TWinControl(FOwnerControl)
  else
    Exit;

  if (LOwnerControl.Parent <> nil) and LOwnerControl.Parent.DoubleBuffered then
    PerformEraseBackground(LOwnerControl.Parent, ACanvas.Handle)
  else
    StyleServices.DrawParentBackground(LOwnerControl.Handle, ACanvas.Handle, nil, False);
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
  else if ((FKind = bkCustom) and Assigned(FGlyph)) or (FKind <> bkCustom) then
  begin
    if Assigned(FGlyph) then
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

procedure TStyledButtonRender.DrawImage(const ACanvas: TCanvas;
  var ATextRect: TRect);
var
  LImageRect: TRect;
  LImageList: TCustomImageList;
  LImageIndex: Integer;
  LImageWidth, LImageHeight: Integer;
begin
  if GetImageSize(LImageWidth, LImageHeight, LImageList, LImageIndex) then
  begin
    LImageRect := CalcImageRect(ATextRect, LImageWidth, LImageHeight);
    if Assigned(LImageList) then
      LImageList.Draw(ACanvas, LImageRect.Left, LImageRect.Top, LImageIndex, Enabled);
  end
  else
  begin
    if ((FKind = bkCustom) and Assigned(FGlyph)) or (FKind <> bkCustom) then
    begin
      if (LImageWidth > 0) and (LImageHeight > 0) then
      begin
        LImageRect := CalcImageRect(ATextRect, LImageWidth, LImageHeight);
        DrawBitBtnGlyph(ACanvas, LImageRect, FKind, FState, Enabled, FGlyph, FNumGlyphs, FTransparentColor);
      end;
    end;
  end;
end;

procedure TStyledButtonRender.DrawButton(const ACanvas: TCanvas);
var
  LTextFlags: Cardinal;
  LTextRect: TRect;
  LOldFontName: TFontName;
  LOldFontColor: TColor;
  LOldFontStyle: TFontStyles;
  LOldParentFont: boolean;
  LStyleAttribute: TStyledButtonAttributes;
begin
  if not (csDesigning in ComponentState) and
   (not FOwnerControl.Visible or (FUpdateCount > 0)) then
    Exit;

  LOldParentFont := ParentFont;
  LOldFontName := Font.Name;
  LOldFontColor := Font.Color;
  LOldFontStyle := Font.Style;

  try
    LStyleAttribute := GetDrawingStyle(ACanvas);
    LTextFlags := 0;
    if FWordWrap then
      LTextFlags := LTextFlags or DT_WORDBREAK or DT_CENTER;

    DrawBackgroundAndBorder(ACanvas, LStyleAttribute);

    LTextRect := FOwnerControl.ClientRect;
    Dec(LTextRect.Right, FDropDownRect.Width);

    DrawImage(ACanvas, LTextRect);

    if LTextRect.IsEmpty then
      LTextRect := FOwnerControl.ClientRect;
    if FOwnerControl.AlignWithMargins then
      InflateRect(LTextRect, -FOwnerControl.Margins.Left-FOwnerControl.Margins.Right,
        -FOwnerControl.Margins.Top-FOwnerControl.Margins.Bottom);
    DrawText(ACanvas, Caption, LTextRect, FOwnerControl.DrawTextBiDiModeFlags(LTextFlags));
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
  if FOwnerControl is TStyledGraphicButton then
    Result := TStyledGraphicButton(FOwnerControl).ButtonState
  else if FOwnerControl is TStyledButton then
    Result := TStyledButton(FOwnerControl).ButtonState
  else
    Result := bsmNormal;
end;

function TStyledButtonRender.GetDrawingStyle(const ACanvas: TCanvas): TStyledButtonAttributes;
begin
  //Getting drawing styles
  Result := GetAttributes(ButtonState);
  ACanvas.Pen.Style := Result.PenStyle;
  ACanvas.Pen.Width := Round(Result.BorderWidth{$IFDEF D10_3+}*FOwnerControl.ScaleFactor{$ENDIF});
  ACanvas.Pen.Color := Result.BorderColor;
  ACanvas.Brush.Style := Result.BrushStyle;
  if ACanvas.Brush.Style <> bsClear then
    ACanvas.Brush.Color := Result.ButtonColor;
  ACanvas.Font := Font;
  ACanvas.Font.Color := Result.FontColor;
  if ParentFont then
    ACanvas.Font.Style := Result.FontStyle;

  if FStyle = bsSplitButton then
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
  FStyleDrawType := AValue;
  FCustomDrawType := True;
(* do not assign DrawType to any Style
    FButtonStyleNormal.DrawType := FDrawType;
    FButtonStylePressed.DrawType := FDrawType;
    FButtonStyleSelected.DrawType := FDrawType;
    FButtonStyleHot.DrawType := FDrawType;
    FButtonStyleDisabled.DrawType := FDrawType;
*)
  Invalidate;
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
    Result := AMargin + Round(AOffset*{$IFDEF D10_3+}FOwnerControl.ScaleFactor{$ELSE}1{$ENDIF});
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
  if (FOwnerControl is TStyledButton) then
    Result := TStyledButton(FOwnerControl).ImageIndex
  else if (FOwnerControl is TStyledGraphicButton) then
    Result := TStyledGraphicButton(FOwnerControl).ImageIndex
  else
    raise EStyledButtonError.Create(ERROR_CANNOT_USE_RENDER);
end;

procedure TStyledButtonRender.SetImageIndex(const AValue: TImageIndex);
begin
  if AValue <> FImageIndex then
  begin
    FImageIndex := AValue;
    if (FOwnerControl is TStyledButton) then
      TStyledButton(FOwnerControl).ImageIndex := AValue
    else if (FOwnerControl is TStyledGraphicButton) then
      TStyledGraphicButton(FOwnerControl).ImageIndex := AValue;

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
  if (FOwnerControl is TStyledButton) then
    Result := TStyledButton(FOwnerControl).FImageName
  else if (FOwnerControl is TStyledGraphicButton) then
    Result := TStyledGraphicButton(FOwnerControl).FImageName
  else
    raise EStyledButtonError.Create(ERROR_CANNOT_USE_RENDER);
end;

procedure TStyledButtonRender.SetImageName(const AValue: TImageName);
begin
  if AValue <> FImageName then
  begin
    FImageName := AValue;
    if (FOwnerControl is TStyledButton) then
      TStyledButton(FOwnerControl).ImageName := AValue
    else if (FOwnerControl is TStyledGraphicButton) then
      TStyledGraphicButton(FOwnerControl).ImageName := AValue;
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
  {$IFDEF D10_4+}
  Result := FOwnerControl.GetStyleName;
  if Result = '' then
  begin
    {$IFDEF D11+}
    if (csDesigning in ComponentState) then
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
end;

procedure TStyledButtonRender.UpdateStyleElements;
var
  LStyleClass: TStyledButtonClass;
begin
  if AsVCLStyle then
  begin
    //if StyleElements contains seClient then Update style
    //as VCL Style assigned to Button or Global VCL Style
    if seBorder in FOwnerControl.StyleElements then
      StyleAppearance := DEFAULT_APPEARANCE;
    LStyleClass := GetActiveStyleName;
    if LStyleClass <> FStyleClass then
    begin
      FStyleClass := LStyleClass;
      StyleApplied := ApplyButtonStyle;
    end;
  end;
  inherited;
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

procedure TStyledButtonRender.SetDropDownMenu(const Value: TPopupMenu);
begin
  if Value <> FDropDownMenu then
  begin
    if DropDownMenu <> nil then
      DropDownMenu.RemoveFreeNotification(FOwnerControl);
    FDropDownMenu := Value;
    if DropDownMenu <> nil then
      DropDownMenu.FreeNotification(FOwnerControl);
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
    TCustomControl(FOwnerControl).CanFocus then
    TCustomControl(FOwnerControl).SetFocus;
end;

procedure TStyledButtonRender.SetGlyph(const AValue: TBitmap);
begin
  if Assigned(AValue) then
    FTransparentColor := AValue.TransparentColor;
  Glyph.Assign(AValue);
  Invalidate;
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

procedure TStyledButtonRender.SetKind(const Value: TBitBtnKind);
begin
  if Value <> FKind then
  begin
    if Value <> bkCustom then
    begin
      Default := Value in [bkOK, bkYes];
      Cancel := Value in [bkCancel, bkNo];

      if ((csLoading in ComponentState) and (Caption = '')) or
        (not (csLoading in ComponentState)) then
      begin
        if Value <> bkCustom then
          Caption := BitBtnCaptions(Value);
      end;

      ModalResult := BitBtnModalResults[Value];
    end;
    FKind := Value;
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
    UpdateStyleElements;
    StyleApplied := ApplyButtonStyle;
  end
  else
    Result := False;
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

procedure TStyledButtonRender.SetState(const AValue: TButtonState);
begin
  FState := AValue;
end;

procedure TStyledButtonRender.SetStyle(const Value: TStyledButtonStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
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
      FOwnerControl.StyleElements := FOwnerControl.StyleElements - [seClient]
    else
      LValue := GetActiveStyleName;
  end;
  if LValue = '' then
    LValue := DEFAULT_WINDOWS_CLASS;
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
  if not FStyleApplied then
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
  FMouseOverDropDown := (FStyle = bsSplitButton) and (X >= FDropDownRect.Left);
end;

procedure TStyledButtonRender.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then
  begin
    State := bsUp;
    Invalidate;
    inherited;
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

function TStyledButtonRender.GetName: TComponentName;
begin
  Result := FOwnerControl.Name;
end;

function TStyledButtonRender.GetNumGlyphs: TNumGlyphs;
begin
  Result := FNumGlyphs;
end;

procedure TStyledGraphicButton.SetNumGlyphs(const AValue: TNumGlyphs);
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

function TStyledButtonRender.GetGlyph: TBitmap;
begin
  if not Assigned(FGlyph) then
    FGlyph := TBitmap.Create;
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
  FOwnerControl.Invalidate;
end;

{ TStyledGraphicButton }

procedure TStyledGraphicButton.AssignStyleTo(ADestRender: TStyledButtonRender);
begin
  FRender.AssignStyleTo(ADestRender);
end;

procedure TStyledGraphicButton.AssignStyleTo(ADest: TStyledGraphicButton);
begin
  FRender.AssignStyleTo(ADest.Render);
end;

function TStyledGraphicButton.AssignAttributes(
  const AEnabled: Boolean = True;
  const AImageList: TCustomImageList = nil;
  {$IFDEF D10_4+}const AImageName: string = '';{$ENDIF}
  const AImageIndex: Integer = -1;
  const AImageAlignment: TImageAlignment = iaLeft;
  const AAction: TCustomAction = nil;
  const AOnClick: TNotifyEvent = nil;
  const AName: string = ''): TStyledGraphicButton;
begin
  Result := FRender.AssignAttributes(AEnabled,
    AImageList,
    {$IFDEF D10_4+}AImageName,{$ENDIF}
    AImageIndex,
    AImageAlignment,
    AAction,
    AOnClick,
    AName) as TStyledGraphicButton;
end;

procedure TStyledGraphicButton.AssignTo(ADest: TPersistent);
var
  LDest: TStyledGraphicButton;
begin
  inherited AssignTo(ADest);
  if ADest is TStyledGraphicButton then
  begin
    LDest := TStyledGraphicButton(ADest);
    FRender.AssignStyleTo(LDest.Render);
    LDest.Hint := Self.Hint;
    LDest.Visible := Self.Visible;
    LDest.Caption := Self.Caption;
    LDest.ModalResult := Self.ModalResult;
    LDest.Tag := Self.Tag;
    LDest.Enabled := Self.Enabled;
    LDest.Hint := Self.Hint;
    LDest.Visible := Self.Visible;
  end;
end;

procedure TStyledGraphicButton.BeginUpdate;
begin
  FRender.BeginUpdate;
end;

procedure TStyledGraphicButton.EndUpdate;
begin
  FRender.EndUpdate;
end;

{$IFDEF HiDPISupport}
procedure TStyledGraphicButton.ChangeScale(M, D: Integer; isDpiChange: Boolean);
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

procedure TStyledGraphicButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Visible then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TStyledGraphicButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FRender.CMEnabledChanged(Message);
end;

procedure TStyledGraphicButton.CMEnter(var Message: TCMEnter);
begin
  inherited;
  FRender.CMEnter(Message);
end;

procedure TStyledGraphicButton.CMMouseEnter(var Message: TNotifyEvent);
begin
  inherited;
  FRender.CMMouseEnter(Message);
end;

procedure TStyledGraphicButton.CMMouseLeave(var Message: TNotifyEvent);
begin
  inherited;
  FRender.CMMouseLeave(Message);
end;

procedure TStyledGraphicButton.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  FRender.CMStyleChanged(Message);
end;

procedure TStyledGraphicButton.Click;
begin
  FRender.Click(False);
end;

constructor TStyledGraphicButton.CreateStyled(AOwner: TComponent;
  const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance);
begin
  inherited Create(AOwner);
  FImageIndex := -1;
  {$IFDEF D10_4+}
  FImageName := '';
  {$ENDIF}
  FRender := GetRenderClass.CreateStyled(Self,
    ControlClick, ControlFont, GetCaption, SetCaption,
      GetParentFont, SetParentFont,
      AFamily, AClass, AAppearance);
end;

constructor TStyledGraphicButton.Create(AOwner: TComponent);
begin
  CreateStyled(AOwner,
    DEFAULT_CLASSIC_FAMILY,
    DEFAULT_WINDOWS_CLASS,
    DEFAULT_APPEARANCE);
end;

procedure TStyledGraphicButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited;
  FRender.ActionChange(Sender, CheckDefaults);
end;

destructor TStyledGraphicButton.Destroy;
begin
  FreeAndNil(FRender);
  inherited Destroy;
end;

procedure TStyledGraphicButton.DoDropDownMenu;
begin
  FRender.DoDropDownMenu;
end;

function TStyledGraphicButton.GetText: TCaption;
begin
  Result := FRender.GetText;
end;

function TStyledGraphicButton.GetKind: TBitBtnKind;
begin
  Result := FRender.Kind;
end;

function TStyledGraphicButton.ImageMarginsStored: Boolean;
begin
  Result := not FRender.IsDefaultImageMargins;
end;

function TStyledGraphicButton.IsCaptionStored: Boolean;
begin
  if (ActionLink = nil) then
    Result := Caption <> ''
  else
    Result := not TGraphicButtonActionLink(ActionLink).IsCaptionLinked;
end;

function TStyledGraphicButton.IsEnabledStored: Boolean;
begin
  if (ActionLink = nil) then
    Result := not Enabled
  else
    Result := not TGraphicButtonActionLink(ActionLink).IsEnabledLinked;
end;

function TStyledGraphicButton.IsImageIndexStored: Boolean;
begin
  if (ActionLink = nil) then
    Result := FImageIndex <> -1
  else
    Result := not TGraphicButtonActionLink(ActionLink).IsImageIndexLinked;
end;

function TStyledGraphicButton.IsCustomDrawType: Boolean;
begin
  Result := FRender.IsCustomDrawType;
end;

function TStyledGraphicButton.IsCustomRadius: Boolean;
begin
  Result := FRender.IsCustomRadius;
end;

function TStyledGraphicButton.IsStoredStyleFamily: Boolean;
begin
  Result := FRender.IsStoredStyleFamily;
end;

function TStyledGraphicButton.IsStoredStyleClass: Boolean;
begin
  Result := FRender.IsStoredStyleClass;
end;

function TStyledGraphicButton.IsStoredStyleAppearance: Boolean;
begin
  Result := FRender.IsStoredStyleAppearance;
end;

function TStyledGraphicButton.IsStoredStyleElements: Boolean;
begin
  Result := FRender.IsStoredStyleElements;
end;

function TStyledGraphicButton.IsStyleDisabledStored: Boolean;
begin
  Result := FRender.IsStyleDisabledStored;
end;

function TStyledGraphicButton.IsStylePressedStored: Boolean;
begin
  Result := FRender.IsStylePressedStored;
end;

function TStyledGraphicButton.IsStyleSelectedStored: Boolean;
begin
  Result := FRender.IsStyleSelectedStored;
end;

function TStyledGraphicButton.IsStyleHotStored: Boolean;
begin
  Result := FRender.IsStyleHotStored;
end;

function TStyledGraphicButton.IsStyleNormalStored: Boolean;
begin
  Result := FRender.IsStyleNormalStored;
end;

function TStyledGraphicButton.GetButtonState: TStyledButtonState;
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

function TStyledGraphicButton.GetFlat: Boolean;
begin
  Result := FRender.Flat;
end;

function TStyledGraphicButton.GetFocused: Boolean;
begin
  Result := False;
end;

function TStyledGraphicButton.GetGlyph: TBitmap;
begin
  Result := FRender.Glyph;
end;

procedure TStyledGraphicButton.SetGlyph(const AValue: TBitmap);
begin
  FRender.Glyph := AValue;
end;

procedure TStyledGraphicButton.Paint;
begin
  FRender.DrawButton(Canvas);
end;

function TStyledGraphicButton.GetRenderClass: TStyledButtonRenderClass;
begin
  Result := TStyledButtonRender;
end;

function TStyledGraphicButton.GetRescalingButton: Boolean;
begin
  Result := Assigned(FRender) and FRender.RescalingButton;
end;

procedure TStyledGraphicButton.SetRescalingButton(const AValue: Boolean);
begin
  if Assigned(FRender) then
    FRender.RescalingButton := AValue;
end;

function TStyledGraphicButton.GetStyleDrawType: TStyledButtonDrawType;
begin
  Result := FRender.StyleDrawType;
end;

procedure TStyledGraphicButton.SetStyleDrawType(const AValue: TStyledButtonDrawType);
begin
  FRender.StyleDrawType := AValue;
end;

function TStyledGraphicButton.GetImage(out AImageList: TCustomImageList;
  out AImageIndex: Integer): Boolean;
begin
  Result := FRender.GetImage(AImageList, AImageIndex);
end;

function TStyledGraphicButton.GetImageAlignment: TImageAlignment;
begin
  Result := FRender.ImageAlignment;
end;

procedure TStyledGraphicButton.SetImageAlignment(const AValue: TImageAlignment);
begin
  FRender.ImageAlignment := AValue;
end;

function TStyledGraphicButton.GetDisabledImageIndex: TImageIndex;
begin
  Result := FRender.DisabledImageIndex;
end;

procedure TStyledGraphicButton.SetDisabledImageIndex(const AValue: TImageIndex);
begin
  FRender.DisabledImageIndex := AValue;
end;

procedure TStyledGraphicButton.SetImageIndex(const AValue: TImageIndex);
begin
  if AValue <> FImageIndex then
  begin
    FImageIndex := AValue;
    FRender.ImageIndex := AValue;
  end;
end;

{$IFDEF D10_4+}
function TStyledGraphicButton.GetDisabledImageName: TImageName;
begin
  Result := FRender.DisabledImageName;
end;

procedure TStyledGraphicButton.SetDisabledImageName(const AValue: TImageName);
begin
  FRender.DisabledImageName := AValue;
end;

function TStyledGraphicButton.IsImageNameStored: Boolean;
begin
  Result := (ActionLink = nil) or
    not TGraphicButtonActionLink(ActionLink).IsImageNameLinked;
end;

function TStyledGraphicButton.GetImageName: TImageName;
begin
  Result := FImageName;
end;

procedure TStyledGraphicButton.SetImageName(const AValue: TImageName);
begin
  if AValue <> FImageName then
  begin
    FImageName := AValue;
    FRender.ImageName := AValue;
  end;
end;

function TStyledGraphicButton.GetHotImageName: TImageName;
begin
  Result := FRender.HotImageName;
end;

procedure TStyledGraphicButton.SetHotImageName(const AValue: TImageName);
begin
  FRender.HotImageName := AValue;
end;

function TStyledGraphicButton.GetPressedImageName: TImageName;
begin
  Result := FRender.PressedImageName;
end;

procedure TStyledGraphicButton.SetPressedImageName(const AValue: TImageName);
begin
  FRender.PressedImageName := AValue;
end;

function TStyledGraphicButton.GetSelectedImageName: TImageName;
begin
  Result := FRender.SelectedImageName;
end;

procedure TStyledGraphicButton.SetSelectedImageName(const AValue: TImageName);
begin
  FRender.SelectedImageName := AValue;
end;
{$ENDIF}

function TStyledGraphicButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TGraphicButtonActionLink;
end;
(*
function TStyledGraphicButton.GetActiveStyleName: string;
begin
  Result := FRender.ActiveStyleName;
end;
*)
procedure TStyledGraphicButton.UpdateStyleElements;
begin
  FRender.UpdateStyleElements;
  inherited;
end;

function TStyledGraphicButton.GetDisabledImages: TCustomImageList;
begin
  Result := FRender.DisabledImages;
end;

procedure TStyledGraphicButton.SetDisabledImages(const AValue: TCustomImageList);
begin
  FRender.DisabledImages := AValue;
end;

function TStyledGraphicButton.GetDropDownMenu: TPopupMenu;
begin
  Result := FRender.DropDownMenu;
end;

procedure TStyledGraphicButton.SetDropDownMenu(const AValue: TPopupMenu);
begin
  FRender.DropDownMenu := AValue;
end;

procedure TStyledGraphicButton.SetFlat(const AValue: Boolean);
begin
  FRender.Flat := AValue;
end;

function TStyledGraphicButton.GetHotImageIndex: TImageIndex;
begin
  Result := FRender.HotImageIndex;
end;

procedure TStyledGraphicButton.SetHotImageIndex(const AValue: TImageIndex);
begin
  FRender.HotImageIndex := AValue;
end;

function TStyledGraphicButton.GetImages: TCustomImageList;
begin
  Result := FRender.Images;
end;

procedure TStyledGraphicButton.SetImages(const AValue: TCustomImageList);
begin
  FRender.Images := AValue;
end;

procedure TStyledGraphicButton.SetKind(const AValue: TBitBtnKind);
begin
  FRender.Kind := AValue;
end;

function TStyledGraphicButton.GetModalResult: TModalResult;
begin
  Result := FRender.ModalResult;
end;

function TStyledGraphicButton.GetMouseInControl: Boolean;
begin
  Result := FRender.MouseInControl;
end;

function TStyledGraphicButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := FRender.NumGlyphs;
end;

procedure TStyledGraphicButton.SetModalResult(const AValue: TModalResult);
begin
  FRender.ModalResult := AValue;
end;

procedure TStyledGraphicButton.SetName(const AValue: TComponentName);
var
  LOldValue: string;
begin
  LOldValue := Caption;
  inherited;
  if LOldValue <> Caption then
    Invalidate;
end;

function TStyledGraphicButton.GetOnDropDownClick: TNotifyEvent;
begin
  Result := FRender.OnDropDownClick;
end;

procedure TStyledGraphicButton.SetOnDropDownClick(const AValue: TNotifyEvent);
begin
  FRender.OnDropDownClick := AValue;
end;

function TStyledGraphicButton.GetPressedImageIndex: TImageIndex;
begin
  Result := FRender.PressedImageIndex;
end;

procedure TStyledGraphicButton.SetPressedImageIndex(const AValue: TImageIndex);
begin
  FRender.PressedImageIndex := AValue;
end;

function TStyledGraphicButton.GetButtonStyleNormal: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStyleNormal;
end;

procedure TStyledGraphicButton.SetButtonStyleNormal(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStyleNormal := AValue;
end;

procedure TStyledGraphicButton.SetButtonStyle(
  const AStyleFamily: TStyledButtonFamily;
  const AStyleClass: TStyledButtonClass;
  const AStyleAppearance: TStyledButtonAppearance);
begin
  FRender.SetButtonStyle(AStyleFamily, AStyleClass, AStyleAppearance);
end;

procedure TStyledGraphicButton.SetButtonStyle(const AStyleFamily: TStyledButtonFamily;
  const AModalResult: TModalResult);
begin
  FRender.SetButtonStyle(AStyleFamily, AModalResult);
end;

function TStyledGraphicButton.GetButtonStyleDisabled: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStyleDisabled;
end;

procedure TStyledGraphicButton.SetButtonStyleDisabled(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStyleDisabled := AValue;
end;

function TStyledGraphicButton.GetButtonStylePressed: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStylePressed;
end;

procedure TStyledGraphicButton.SetButtonStylePressed(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStylePressed := AValue;
end;

function TStyledGraphicButton.GetButtonStyleSelected: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStyleSelected;
end;

procedure TStyledGraphicButton.SetButtonStyleSelected(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStyleSelected := AValue;
end;

function TStyledGraphicButton.GetButtonStyleHot: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStyleHot;
end;

procedure TStyledGraphicButton.SetButtonStyleHot(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStyleHot := AValue;
end;

function TStyledGraphicButton.GetImageMargins: TImageMargins;
begin
  Result := FRender.ImageMargins;
end;

procedure TStyledGraphicButton.SetImageMargins(const AValue: TImageMargins);
begin
  FRender.ImageMargins := AValue;
end;

function TStyledGraphicButton.GetStyleRadius: Integer;
begin
  Result := FRender.StyleRadius;
end;

procedure TStyledGraphicButton.SetStyleRadius(const AValue: Integer);
begin
  FRender.StyleRadius := AValue;
end;

function TStyledGraphicButton.GetSelectedImageIndex: TImageIndex;
begin
  Result := FRender.SelectedImageIndex;
end;

function TStyledGraphicButton.GetSplitButtonWidth: Integer;
begin
  Result := FRender.GetSplitButtonWidth;
end;

procedure TStyledGraphicButton.SetSelectedImageIndex(const AValue: TImageIndex);
begin
  FRender.SelectedImageIndex := AValue;
end;

function TStyledGraphicButton.GetStyle: TStyledButtonStyle;
begin
  Result := FRender.Style;
end;

procedure TStyledGraphicButton.SetStyle(const AValue: TStyledButtonStyle);
begin
  FRender.Style := AValue;
end;

function TStyledGraphicButton.GetStyleAppearance: TStyledButtonAppearance;
begin
  Result := FRender.StyleAppearance;
end;

procedure TStyledGraphicButton.SetStyleAppearance(const AValue: TStyledButtonAppearance);
begin
  FRender.StyleAppearance := AValue;
end;

function TStyledGraphicButton.GetStyleApplied: Boolean;
begin
  Result := FRender.StyleApplied;
end;

procedure TStyledGraphicButton.SetStyleApplied(const AValue: Boolean);
begin
  FRender.StyleApplied := AValue;
end;

function TStyledGraphicButton.GetStyleClass: TStyledButtonClass;
begin
  Result := FRender.StyleClass;
end;

procedure TStyledGraphicButton.SetStyleClass(const AValue: TStyledButtonClass);
begin
  FRender.StyleClass := AValue;
end;

function TStyledGraphicButton.GetStyleFamily: TStyledButtonFamily;
begin
  Result := FRender.StyleFamily;
end;

procedure TStyledGraphicButton.SetStyleFamily(const AValue: TStyledButtonFamily);
begin
  FRender.StyleFamily := AValue;
end;

procedure TStyledGraphicButton.SetText(const AValue: TCaption);
begin
  FRender.Caption := AValue;
end;

function TStyledGraphicButton.GetWordWrap: Boolean;
begin
  Result := FRender.WordWrap;
end;

function TStyledGraphicButton.HasCustomGlyph: Boolean;
var
  Link: TGraphicButtonActionLink;
begin
  Link := TGraphicButtonActionLink(ActionLink);
  Result := not ((Link <> nil) and Link.IsImageIndexLinked and
    Link.IsGlyphLinked(ImageIndex));
end;

procedure TStyledGraphicButton.SetWordWrap(const AValue: Boolean);
begin
  FRender.WordWrap := AValue;
end;

procedure TStyledGraphicButton.ShowDropDownMenu;
begin
  FRender.ShowDropDownMenu;
end;

procedure TStyledGraphicButton.Loaded;
begin
  inherited;
  FRender.Loaded;
end;

procedure TStyledGraphicButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FRender.MouseDown(Button, Shift, X, Y);
  if Enabled then
    inherited;
end;

procedure TStyledGraphicButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FRender.MouseMove(Shift, X, Y);
end;

procedure TStyledGraphicButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then
  begin
    FRender.MouseUp(Button, Shift, X, Y);
    inherited;
  end;
end;

procedure TStyledGraphicButton.ControlClick(Sender: TObject);
begin
  inherited Click;
end;

procedure TStyledGraphicButton.ControlFont(var AValue: TFont);
begin
  AValue := Self.Font;
end;

procedure TStyledGraphicButton.SetParentFont(const AValue: Boolean);
begin
  Self.ParentFont := AValue;
end;

function TStyledGraphicButton.GetParentFont: Boolean;
begin
  Result := Self.ParentFont;
end;

function TStyledGraphicButton.GetCaption: TCaption;
begin
  Result := inherited Caption;
end;

function TStyledGraphicButton.GetCursor: TCursor;
begin
  Result := inherited Cursor;
end;

procedure TStyledGraphicButton.SetCaption(const AValue: TCaption);
begin
  inherited Caption := AValue;
end;

procedure TStyledGraphicButton.SetCursor(const AValue: TCursor);
begin
  if AValue <> Cursor then
  begin
    inherited Cursor := AValue;
  end;
end;

procedure TStyledGraphicButton.Notification(AComponent: TComponent; AOperation: TOperation);
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

function TStyledGraphicButton.GetTag: Integer;
begin
  Result := FRender.Tag;
end;

procedure TStyledGraphicButton.SetTag(const AValue: Integer);
begin
  FRender.Tag := AValue;
end;

{ TGraphicButtonActionLink }

procedure TGraphicButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TControl;
end;

function TGraphicButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked;
   (*and (FClient.Checked = TCustomAction(Action).Checked);*)
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
  if FClient is TStyledGraphicButton then
  begin
    LGlyph := TStyledGraphicButton(FClient).Glyph;
    LRender := TStyledGraphicButton(FClient).Render;
  end
  else if FClient is TStyledButton then
  begin
    LGlyph := TStyledButton(FClient).Glyph;
    LRender := TStyledButton(FClient).Render;
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
  if FClient is TStyledButton then
    Result := inherited IsImageIndexLinked and
      (TStyledButton(FClient).ImageIndex =
        TCustomAction(Action).ImageIndex)
  else if FClient is TStyledGraphicButton then
    Result := inherited IsImageIndexLinked and
      (TStyledGraphicButton(FClient).ImageIndex =
        TCustomAction(Action).ImageIndex)
  else
    Result := False;
end;

{$IFDEF D10_4+}
function TGraphicButtonActionLink.IsImageNameLinked: Boolean;
begin
  Result := inherited IsImageNameLinked and
    (TStyledGraphicButton(FClient).ImageName =
      TCustomAction(Action).ImageName);
end;
{$ENDIF}

procedure TGraphicButtonActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then
    FClient.Enabled := Value;
end;

procedure TGraphicButtonActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
  begin
    if (FClient is TStyledButton) then
      TStyledButton(FClient).ImageIndex := Value
    else if (FClient is TStyledGraphicButton) then
      TStyledGraphicButton(FClient).ImageIndex := Value;
  end;
end;

{ TStyledButton }

procedure TStyledButton.AssignStyleTo(ADestRender: TStyledButtonRender);
begin
  FRender.AssignStyleTo(ADestRender);
end;

procedure TStyledButton.AssignStyleTo(ADest: TStyledButton);
begin
  FRender.AssignStyleTo(ADest.Render);
end;

function TStyledButton.AssignAttributes(
  const AEnabled: Boolean = True;
  const AImageList: TCustomImageList = nil;
  {$IFDEF D10_4+}const AImageName: string = '';{$ENDIF}
  const AImageIndex: Integer = -1;
  const AImageAlignment: TImageAlignment = iaLeft;
  const AAction: TCustomAction = nil;
  const AOnClick: TNotifyEvent = nil;
  const AName: string = ''): TStyledButton;
begin
  Result := FRender.AssignAttributes(AEnabled,
    AImageList,
    {$IFDEF D10_4+}AImageName,{$ENDIF}
    AImageIndex,
    AImageAlignment,
    AAction,
    AOnClick,
    AName) as TStyledButton;
end;

procedure TStyledButton.AssignTo(ADest: TPersistent);
var
  LDest: TStyledButton;
begin
  inherited AssignTo(ADest);
  if ADest is TStyledButton then
  begin
  if ADest is TStyledButton then
  begin
    LDest := TStyledButton(ADest);
    FRender.AssignStyleTo(LDest.Render);
    LDest.Hint := Self.Hint;
    LDest.Visible := Self.Visible;
    LDest.Caption := Self.Caption;
    LDest.ModalResult := Self.ModalResult;
    LDest.Tag := Self.Tag;
    LDest.Enabled := Self.Enabled;
    LDest.Hint := Self.Hint;
    LDest.Visible := Self.Visible;
    LDest.TabStop := Self.TabStop;
  end;
  end;
end;

procedure TStyledButton.BeginUpdate;
begin
  FRender.BeginUpdate;
end;

procedure TStyledButton.EndUpdate;
begin
  FRender.EndUpdate;
end;

{$IFDEF HiDPISupport}
procedure TStyledButton.ChangeScale(M, D: Integer; isDpiChange: Boolean);
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

procedure TStyledButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FRender.CMEnabledChanged(Message);
end;

procedure TStyledButton.CMEnter(var Message: TCMEnter);
begin
  inherited;
  FRender.CMEnter(Message);
end;

procedure TStyledButton.CMMouseEnter(var Message: TNotifyEvent);
begin
  inherited;
  FRender.CMMouseEnter(Message);
end;

procedure TStyledButton.CMMouseLeave(var Message: TNotifyEvent);
begin
  inherited;
  FRender.CMMouseLeave(Message);
end;

procedure TStyledButton.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  FRender.CMStyleChanged(Message);
end;

constructor TStyledButton.CreateStyled(AOwner: TComponent;
  const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  ParentColor := False;
  FImageIndex := -1;
  {$IFDEF D10_4+}
  FImageName := '';
  {$ENDIF}
    FRender := GetRenderClass.CreateStyled(Self,
    ControlClick, ControlFont, GetCaption, SetCaption,
      GetParentFont, SetParentFont, AFamily, AClass, AAppearance);
  TabStop := True;
end;

procedure TStyledButton.CreateWnd;
begin
  inherited CreateWnd;
  FRender.Active := Default;
(*
  if not (csLoading in ComponentState) then
  begin
    SetElevationRequiredState;
    UpdateImageList;
    if FStyle = bsCommandLink then
      UpdateCommandLinkHint;
  end;
*)
end;

constructor TStyledButton.Create(AOwner: TComponent);
begin
  CreateStyled(AOwner,
    DEFAULT_CLASSIC_FAMILY,
    DEFAULT_WINDOWS_CLASS,
    DEFAULT_APPEARANCE);
end;

procedure TStyledButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited;
  FRender.ActionChange(Sender, CheckDefaults);
end;

destructor TStyledButton.Destroy;
begin
  FreeAndNil(FRender);
  inherited Destroy;
end;

function TStyledButton.CalcImageRect(var ATextRect: TRect; const AImageWidth,
  AImageHeight: Integer): TRect;
begin
  Result := FRender.CalcImageRect(ATextRect, AImageWidth, AImageHeight);
end;

function TStyledButton.CanDropDownMenu: boolean;
begin
  Result := FRender.CanDropDownMenu;
end;

procedure TStyledButton.DoDropDownMenu;
begin
  FRender.DoDropDownMenu;
end;

function TStyledButton.GetText: TCaption;
begin
  Result := FRender.GetText;
end;

function TStyledButton.GetKind: TBitBtnKind;
begin
  Result := FRender.Kind;
end;

function TStyledButton.ImageMarginsStored: Boolean;
begin
  Result := not FRender.IsDefaultImageMargins;
end;

function TStyledButton.IsCaptionStored: Boolean;
begin
  if (ActionLink = nil) then
    Result := Caption <> ''
  else
    Result := not TGraphicButtonActionLink(ActionLink).IsCaptionLinked;
end;

function TStyledButton.IsEnabledStored: Boolean;
begin
  if (ActionLink = nil) then
    Result := not Enabled
  else
    Result := not TGraphicButtonActionLink(ActionLink).IsEnabledLinked;
end;

function TStyledButton.IsImageIndexStored: Boolean;
begin
  if (ActionLink = nil) then
    Result := ImageIndex <> -1
  else
    Result := not TGraphicButtonActionLink(ActionLink).IsImageIndexLinked;
end;

function TStyledButton.IsCustomDrawType: Boolean;
begin
  Result := FRender.IsCustomDrawType;
end;

function TStyledButton.IsCustomRadius: Boolean;
begin
  Result := FRender.IsCustomRadius;
end;

function TStyledButton.IsStoredStyleFamily: Boolean;
begin
  Result := FRender.IsStoredStyleFamily;
end;

function TStyledButton.IsStoredStyleClass: Boolean;
begin
  Result := FRender.IsStoredStyleClass;
end;

function TStyledButton.IsStoredStyleAppearance: Boolean;
begin
  Result := FRender.IsStoredStyleAppearance;
end;

function TStyledButton.IsStoredStyleElements: Boolean;
begin
  Result := FRender.IsStoredStyleElements;
end;

function TStyledButton.IsStyleDisabledStored: Boolean;
begin
  Result := FRender.IsStyleDisabledStored;
end;

function TStyledButton.IsStylePressedStored: Boolean;
begin
  Result := FRender.IsStylePressedStored;
end;

function TStyledButton.IsStyleSelectedStored: Boolean;
begin
  Result := FRender.IsStyleSelectedStored;
end;

function TStyledButton.IsStyleHotStored: Boolean;
begin
  Result := FRender.IsStyleHotStored;
end;

function TStyledButton.IsStyleNormalStored: Boolean;
begin
  Result := FRender.IsStyleNormalStored;
end;

function TStyledButton.GetButtonState: TStyledButtonState;
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

function TStyledButton.GetRenderClass: TStyledButtonRenderClass;
begin
  Result := TStyledButtonRender;
end;

function TStyledButton.GetRescalingButton: Boolean;
begin
  Result := Assigned(FRender) and FRender.RescalingButton;
end;

procedure TStyledButton.SetRescalingButton(const AValue: Boolean);
begin
  if Assigned(FRender) then
    FRender.RescalingButton := AValue;
end;

function TStyledButton.GetStyleDrawType: TStyledButtonDrawType;
begin
  Result := FRender.StyleDrawType;
end;

procedure TStyledButton.SetStyleDrawType(const AValue: TStyledButtonDrawType);
begin
  FRender.StyleDrawType := AValue;
end;

function TStyledButton.GetImage(out AImageList: TCustomImageList;
  out AImageIndex: Integer): Boolean;
begin
  Result := FRender.GetImage(AImageList, AImageIndex);
end;

function TStyledButton.GetImageAlignment: TImageAlignment;
begin
  Result := FRender.ImageAlignment;
end;

procedure TStyledButton.SetImageAlignment(const AValue: TImageAlignment);
begin
  FRender.ImageAlignment := AValue;
end;

function TStyledButton.GetDefault: Boolean;
begin
  Result := FRender.Default;
end;

procedure TStyledButton.SetDefault(const AValue: Boolean);
var
  Form: TCustomForm;
begin
  FRender.Default := AValue;
  if HandleAllocated then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.Perform(CM_FOCUSCHANGED, 0, LPARAM(Form.ActiveControl));
  end;
  if (csLoading in ComponentState) then
    FRender.Active := FRender.Default;
end;

function TStyledButton.GetCancel: Boolean;
begin
  Result := FRender.Cancel;
end;

procedure TStyledButton.SetCancel(const AValue: Boolean);
begin
  FRender.Cancel := AValue;
end;

function TStyledButton.GetDisabledImageIndex: TImageIndex;
begin
  Result := FRender.DisabledImageIndex;
end;

procedure TStyledButton.SetDisabledImageIndex(const AValue: TImageIndex);
begin
  FRender.DisabledImageIndex := AValue;
end;

procedure TStyledButton.SetImageIndex(const AValue: TImageIndex);
begin
  if AValue <> FImageIndex then
  begin
    FImageIndex := AValue;
    FRender.ImageIndex := AValue;
  end;
end;

{$IFDEF D10_4+}
function TStyledButton.GetDisabledImageName: TImageName;
begin
  Result := FRender.DisabledImageName;
end;

procedure TStyledButton.SetDisabledImageName(const AValue: TImageName);
begin
  FRender.DisabledImageName := AValue;
end;

function TStyledButton.IsImageNameStored: Boolean;
begin
  Result := (ActionLink = nil) or
    not TGraphicButtonActionLink(ActionLink).IsImageNameLinked;
end;

function TStyledButton.GetImageName: TImageName;
begin
  Result := FRender.ImageName;
end;

procedure TStyledButton.SetImageName(const AValue: TImageName);
begin
  if AValue <> FImageName then
  begin
    FImageName := AValue;
    FRender.ImageName := AValue;
  end;
end;

function TStyledButton.GetHotImageName: TImageName;
begin
  Result := FRender.HotImageName;
end;

procedure TStyledButton.SetHotImageName(const AValue: TImageName);
begin
  FRender.HotImageName := AValue;
end;

function TStyledButton.GetPressedImageName: TImageName;
begin
  Result := FRender.PressedImageName;
end;

procedure TStyledButton.SetPressedImageName(const AValue: TImageName);
begin
  FRender.PressedImageName := AValue;
end;

function TStyledButton.GetSelectedImageName: TImageName;
begin
  Result := FRender.SelectedImageName;
end;

procedure TStyledButton.SetSelectedImageName(const AValue: TImageName);
begin
  FRender.SelectedImageName := AValue;
end;
{$ENDIF}

function TStyledButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TGraphicButtonActionLink;
end;
(*
function TStyledButton.GetActiveStyleName: string;
begin
  Result := FRender.ActiveStyleName;
end;
*)
procedure TStyledButton.UpdateStyleElements;
begin
  FRender.UpdateStyleElements;
  inherited;
end;

function TStyledButton.GetDisabledImages: TCustomImageList;
begin
  Result := FRender.DisabledImages;
end;

procedure TStyledButton.SetDisabledImages(const AValue: TCustomImageList);
begin
  FRender.DisabledImages := AValue;
end;

function TStyledButton.GetDropDownMenu: TPopupMenu;
begin
  Result := FRender.DropDownMenu;
end;

function TStyledButton.GetFlat: Boolean;
begin
  Result := FRender.Flat;
end;

function TStyledButton.GetGlyph: TBitmap;
begin
  Result := FRender.Glyph;
end;

procedure TStyledButton.SetGlyph(const AValue: TBitmap);
begin
  FRender.Glyph := AValue;
end;

procedure TStyledButton.SetDropDownMenu(const AValue: TPopupMenu);
begin
  FRender.DropDownMenu := AValue;
end;

procedure TStyledButton.SetFlat(const AValue: Boolean);
begin
  FRender.Flat := AValue;
end;

function TStyledButton.GetHotImageIndex: TImageIndex;
begin
  Result := FRender.HotImageIndex;
end;

procedure TStyledButton.SetHotImageIndex(const AValue: TImageIndex);
begin
  FRender.HotImageIndex := AValue;
end;

function TStyledButton.GetImages: TCustomImageList;
begin
  Result := FRender.Images;
end;

procedure TStyledButton.SetImages(const AValue: TCustomImageList);
begin
  FRender.Images := AValue;
end;

procedure TStyledButton.SetKind(const AValue: TBitBtnKind);
begin
  FRender.Kind := AValue;
end;

function TStyledButton.GetModalResult: TModalResult;
begin
  Result := FRender.ModalResult;
end;

function TStyledButton.GetMouseInControl: Boolean;
begin
  Result := FRender.MouseInControl;
end;

function TStyledButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := FRender.NumGlyphs;
end;

procedure TStyledButton.SetNumGlyphs(const AValue: TNumGlyphs);
begin
  FRender.NumGlyphs := AValue;
end;

procedure TStyledButton.SetModalResult(const AValue: TModalResult);
begin
  FRender.ModalResult := AValue;
end;

procedure TStyledButton.SetName(const AValue: TComponentName);
var
  LOldValue: string;
begin
  LOldValue := Caption;
  inherited;
  if LOldValue <> Caption then
    Invalidate;
end;

function TStyledButton.GetOnDropDownClick: TNotifyEvent;
begin
  Result := FRender.OnDropDownClick;
end;

procedure TStyledButton.SetOnDropDownClick(const AValue: TNotifyEvent);
begin
  FRender.OnDropDownClick := AValue;
end;

function TStyledButton.GetPressedImageIndex: TImageIndex;
begin
  Result := FRender.PressedImageIndex;
end;

procedure TStyledButton.SetPressedImageIndex(const AValue: TImageIndex);
begin
  FRender.PressedImageIndex := AValue;
end;

function TStyledButton.GetButtonStyleNormal: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStyleNormal;
end;

procedure TStyledButton.SetButtonStyleNormal(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStyleNormal := AValue;
end;

procedure TStyledButton.SetButtonStyle(
  const AStyleFamily: TStyledButtonFamily;
  const AStyleClass: TStyledButtonClass;
  const AStyleAppearance: TStyledButtonAppearance);
begin
  FRender.SetButtonStyle(AStyleFamily, AStyleClass, AStyleAppearance);
end;

procedure TStyledButton.SetButtonStyle(const AStyleFamily: TStyledButtonFamily;
  const AModalResult: TModalResult);
begin
  FRender.SetButtonStyle(AStyleFamily, AModalResult);
end;

function TStyledButton.GetButtonStyleDisabled: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStyleDisabled;
end;

procedure TStyledButton.SetButtonStyleDisabled(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStyleDisabled := AValue;
end;

function TStyledButton.GetButtonStylePressed: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStylePressed;
end;

procedure TStyledButton.SetButtonStylePressed(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStylePressed := AValue;
end;

function TStyledButton.GetButtonStyleSelected: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStyleSelected;
end;

procedure TStyledButton.SetButtonStyleSelected(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStyleSelected := AValue;
end;

function TStyledButton.GetButtonStyleHot: TStyledButtonAttributes;
begin
  Result := FRender.ButtonStyleHot;
end;

procedure TStyledButton.SetButtonStyleHot(const AValue: TStyledButtonAttributes);
begin
  FRender.ButtonStyleHot := AValue;
end;

function TStyledButton.GetImageMargins: TImageMargins;
begin
  Result := FRender.ImageMargins;
end;

procedure TStyledButton.SetImageMargins(const AValue: TImageMargins);
begin
  FRender.ImageMargins := AValue;
end;

function TStyledButton.GetStyleRadius: Integer;
begin
  Result := FRender.StyleRadius;
end;

procedure TStyledButton.SetStyleRadius(const AValue: Integer);
begin
  FRender.StyleRadius := AValue;
end;

function TStyledButton.GetSelectedImageIndex: TImageIndex;
begin
  Result := FRender.SelectedImageIndex;
end;

function TStyledButton.GetSplitButtonWidth: Integer;
begin
  Result := FRender.GetSplitButtonWidth;
end;

procedure TStyledButton.SetSelectedImageIndex(const AValue: TImageIndex);
begin
  FRender.SelectedImageIndex := AValue;
end;

function TStyledButton.GetStyle: TStyledButtonStyle;
begin
  Result := FRender.Style;
end;

procedure TStyledButton.SetStyle(const AValue: TStyledButtonStyle);
begin
  FRender.Style := AValue;
end;

function TStyledButton.GetStyleAppearance: TStyledButtonAppearance;
begin
  Result := FRender.StyleAppearance;
end;

procedure TStyledButton.SetStyleAppearance(const AValue: TStyledButtonAppearance);
begin
  FRender.StyleAppearance := AValue;
end;

function TStyledButton.GetStyleApplied: Boolean;
begin
  Result := FRender.StyleApplied;
end;

procedure TStyledButton.SetStyleApplied(const AValue: Boolean);
begin
  FRender.StyleApplied := AValue;
end;

function TStyledButton.GetStyleClass: TStyledButtonClass;
begin
  Result := FRender.StyleClass;
end;

procedure TStyledButton.SetStyleClass(const AValue: TStyledButtonClass);
begin
  FRender.StyleClass := AValue;
end;

function TStyledButton.GetStyleFamily: TStyledButtonFamily;
begin
  Result := FRender.StyleFamily;
end;

procedure TStyledButton.SetStyleFamily(const AValue: TStyledButtonFamily);
begin
  FRender.StyleFamily := AValue;
end;

procedure TStyledButton.SetText(const AValue: TCaption);
begin
  FRender.Caption := AValue;
end;

function TStyledButton.GetWordWrap: Boolean;
begin
  Result := FRender.WordWrap;
end;

function TStyledButton.HasCustomGlyph: Boolean;
var
  Link: TGraphicButtonActionLink;
begin
  Link := TGraphicButtonActionLink(ActionLink);
  Result := not ((Link <> nil) and Link.IsImageIndexLinked and
    Link.IsGlyphLinked(ImageIndex));
end;

procedure TStyledButton.SetWordWrap(const AValue: Boolean);
begin
  FRender.WordWrap := AValue;
end;

procedure TStyledButton.ShowDropDownMenu;
begin
  FRender.ShowDropDownMenu;
end;

procedure TStyledButton.Loaded;
begin
  inherited;
  FRender.Loaded;
end;

procedure TStyledButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FRender.MouseDown(Button, Shift, X, Y);
  if Enabled then
    inherited;
end;

procedure TStyledButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FRender.MouseMove(Shift, X, Y);
end;

procedure TStyledButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then
  begin
    FRender.MouseUp(Button, Shift, X, Y);
    inherited;
  end;
end;

procedure TStyledButton.ControlClick(Sender: TObject);
begin
  inherited Click;
end;

procedure TStyledButton.ControlFont(var AValue: TFont);
begin
  AValue := Self.Font;
end;

procedure TStyledButton.SetParentFont(const AValue: Boolean);
begin
  Self.ParentFont := AValue;
end;

function TStyledButton.GetParentFont: Boolean;
begin
  Result := Self.ParentFont;
end;

function TStyledButton.GetCaption: TCaption;
begin
  Result := inherited Caption;
end;

function TStyledButton.GetCursor: TCursor;
begin
  Result := inherited Cursor;
end;

procedure TStyledButton.SetCaption(const AValue: TCaption);
begin
  inherited Caption := AValue;
end;

procedure TStyledButton.SetCursor(const AValue: TCursor);
begin
  if AValue <> Cursor then
  begin
    inherited Cursor := AValue;
  end;
end;

procedure TStyledButton.Notification(AComponent: TComponent; AOperation: TOperation);
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

function TStyledButton.GetTag: Integer;
begin
  Result := FRender.Tag;
end;

procedure TStyledButton.SetTag(const AValue: Integer);
begin
  FRender.Tag := AValue;
end;

procedure TStyledButton.Click;
begin
  FRender.Click(False);
end;

procedure TStyledButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TStyledButton.CMDialogKey(var Message: TCMDialogKey);
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

(*
function TStyledButton.StyleServicesEnabled: Boolean;
var
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  Result := LStyle.Available and not LStyle.IsSystemStyle and
    (FindControl(Handle) = nil);
end;
*)

procedure TStyledButton.WMEraseBkGnd(var Message: TWmEraseBkgnd);

  function IsComponentStyleActive: Boolean;
  begin
    {$IFDEF D10_4+}
    Result := IsCustomStyleActive;
    {$ELSE}
    Result := False;
    {$ENDIF}
  end;

begin
  if IsComponentStyleActive and (seClient in StyleElements) then
  begin
    Message.Result := 1
  end
  else
  begin
    { Erase background if we're not doublebuffering or painting to memory. }
    if not FDoubleBuffered or
       (TMessage(Message).wParam = WPARAM(TMessage(Message).lParam)) then
    begin
      Brush.Color := FRender.GetBackGroundColor;
      Brush.Style := bsSolid;
      FillRect(Message.DC, ClientRect, Brush.Handle);
    end;
    Message.Result := 1;
  end;
end;

procedure TStyledButton.CNKeyDown(var Message: TWMKeyDown);
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

procedure TStyledButton.WMKeyDown(var Message: TMessage);
begin
  inherited;
  FRender.WMKeyDown(Message);
end;

procedure TStyledButton.WMKeyUp(var Message: TMessage);
begin
  inherited;
  FRender.WMKeyUp(Message);
end;

procedure TStyledButton.CMFocusChanged(var Message: TCMFocusChanged);
begin
  with Message do
    if Sender is TStyledButton then
      FRender.Active := Sender = Self
    else
      FRender.Active := Default;
  inherited;
end;

procedure TStyledButton.WMSetFocus(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TStyledButton.WMKillFocus(var Message: TMessage);
begin
  Invalidate;
  inherited;
end;

procedure TStyledButton.WMPaint(var Message: TMessage);
var
  DC: HDC;
  LCanvas: TCanvas;
  PS: TPaintStruct;
  LControl: TWinControl;
  FPaintBuffer: TBitmap;
begin
  LControl := Self;
  DC := HDC(Message.WParam);
  LCanvas := TCanvas.Create;
  try
    if DC <> 0 then
      LCanvas.Handle := DC
    else
      LCanvas.Handle := BeginPaint(LControl.Handle, PS);

      if FDoubleBuffered and (DC = 0) then
      begin
        FPaintBuffer := TBitmap.Create;
        try
          FPaintBuffer.SetSize(LControl.Width, LControl.Height);
          FRender.DrawButton(FPaintBuffer.Canvas);
          LCanvas.Draw(0, 0, FPaintBuffer);
        finally
          FPaintBuffer.Free;
        end;
      end
     else
       begin
         FRender.DrawButton(LCanvas);
       end;
    if DC = 0 then
      EndPaint(LControl.Handle, PS);
  finally
    LCanvas.Handle := 0;
    LCanvas.Free;
  end;
  FHandled := True;
end;

end.
