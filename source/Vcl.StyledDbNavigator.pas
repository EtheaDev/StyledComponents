{******************************************************************************}
{                                                                              }
{  StyledDbNavigator: a DbNavigator with TStyledNavButtons inside              }
{  Based on TStyledToolbar                                                     }
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
unit Vcl.StyledDbNavigator;

interface

{$INCLUDE StyledComponents.inc}

{$IFDEF D10_4+}
  {$R StyledNavButtonsPNG.RES}
{$ELSE}
  {$R StyledNavButtonsBMP.RES}
{$ENDIF}

uses
  Vcl.ImgList
  , System.UITypes
  , System.SysUtils
  , System.Classes
  , System.Math
  , Vcl.ToolWin
  , Vcl.ComCtrls
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  , Vcl.Themes
  , Vcl.Controls
  , Vcl.ActnList
  , Vcl.Menus
  , Winapi.Messages
  , Winapi.Windows
  , Vcl.StyledButton
  , Vcl.ButtonStylesAttributes
  , Vcl.StandardButtonStyles
  , Vcl.DBCtrls
  , Data.db
  , Data.Bind.Components
  , Data.Bind.Controls
{$IFDEF D10_4+}
  , Vcl.VirtualImageList
  , Vcl.ImageCollection
{$ENDIF}
  ;

resourcestring
  ERROR_SETTING_DBNAVIGATOR_STYLE = 'Error setting DbNavigator Style: %s/%s/%s not available';

const
  NavigatorDefaultBtns = [
    TNavigateBtn.nbFirst, TNavigateBtn.nbPrior, TNavigateBtn.nbNext,
    TNavigateBtn.nbLast, TNavigateBtn.nbInsert, TNavigateBtn.nbDelete,
    TNavigateBtn.nbEdit, TNavigateBtn.nbPost, TNavigateBtn.nbCancel,
    TNavigateBtn.nbRefresh];

  NavigatorMoveBtns = [TNavigateBtn.nbFirst..TNavigateBtn.nbLast];

type
  EStyledDbnavigatorError = Exception;

  TCustomStyledDBNavigator = class;
  TStyledDbNavigator = class;
  TStyledNavButton = class;
  TStyledNavDataLink = class;

  TEnableNavBtnProc = procedure(const ADbNavigator: TCustomStyledDBNavigator; const ABtn: TStyledNavButton; var AEnabled: Boolean) of Object;
  TButtonProc = reference to procedure (Button: TStyledNavButton);
  TNavButtons = array[TNavigateBtn] of TStyledNavButton;

  TNavigateButtonEvent = procedure (Sender: TObject; Button: TNavigateButton) of object;
  TNavigatorOrientation = (orHorizontal, orVertical);

  { TStyledNavButton }
  TStyledNavButton = class(TStyledGraphicButton)
  private
    FIndex: TNavigateBtn;
    FNavStyle: TNavButtonStyle;
    FRepeatTimer: TTimer;
    FDbNavigator: TCustomStyledDBNavigator;
    FImageAlignment: TImageAlignment;
    FDragging: Boolean;
    procedure TimerExpired(Sender: TObject);
    procedure UpdateButtonContent;
    function IsImageAlignmentStored: Boolean;
    procedure SetImageAlignment(const AValue: TImageAlignment);
    function IsCustomDrawType: Boolean;
    function IsCustomRadius: Boolean;
    function IsCustomRoundedCorners: Boolean;
    function IsStoredStyleFamily: Boolean;
    function IsStoredStyleAppearance: Boolean;
  protected
    function IsStoredStyleClass: Boolean; override;
    function GetCaptionToDraw: TCaption; override;
    procedure SetCaption(const AValue: TCaption); override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    procedure EnabledNavBtn(AValue: Boolean);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property NavStyle: TNavButtonStyle read FNavStyle write FNavStyle;
    property Index : TNavigateBtn read FIndex write FIndex;
  published
    property ImageAlignment: TImageAlignment read FImageAlignment write SetImageAlignment Stored IsImageAlignmentStored;
    property Caption: TCaption read GetCaption write SetCaption stored IsCaptionStored;
    //StyledComponents Attributes
    property StyleRadius stored IsCustomRadius;
    property StyleRoundedCorners stored IsCustomRoundedCorners;
    property StyleDrawType stored IsCustomDrawType;
    property StyleFamily stored IsStoredStyleFamily;
    property StyleClass stored IsStoredStyleClass;
    property StyleAppearance stored IsStoredStyleAppearance;
    property ButtonStyleNormal;
    property ButtonStylePressed;
    property ButtonStyleSelected;
    property ButtonStyleHot;
    property ButtonStyleDisabled;
    property NotificationBadge;
    property OnDropDownClick;
  end;

  { TCustomStyledDBNavigator }
  TCustomStyledDBNavigator = class(TCustomPanel)
  private
    //Standard support as TDbNavigator
    FVisibleButtons: TNavButtonSet;
    FHints: TStrings;
    FCaptions: TStrings;
    FDefaultHints: TStrings;
    FDefaultCaptions: TStrings;
    FButtonWidth: Integer;
    FButtonHeight: Integer;
    FMinBtnSize: TPoint;
    FOnNavClick: ENavClick;
    FBeforeAction: ENavClick;
    FocusedButton: TNavigateBtn;
    FConfirmDelete: Boolean;
    FFlat: Boolean;
    FMaxErrors: Integer;
    FKind: TDBNavigatorKind;
    FButtons: TNavButtons;

    //ImageList Support
    FImageChangeLink: TChangeLink;
    FDisabledImageChangeLink: TChangeLink;

    //Styled Attributes
    FStyleRadius: Integer;
    FStyleDrawType: TStyledButtonDrawType;
    FStyleFamily: TStyledButtonFamily;
    FStyleClass: TStyledButtonClass;
    FStyleAppearance: TStyledButtonAppearance;
    FCustomDrawType: Boolean;
    FStyleApplied: Boolean;

    FNoCopyCursor: Boolean;

    //Imagelist support
    FImages: TCustomImageList;
    FDisabledImages: TCustomImageList;
    FShowCaptions: Boolean;
    FOnEnableNavBtn: TEnableNavBtnProc;

    {$IFDEF D10_4+}
    FButtonImages: TVirtualImageList;

    //Internal ImageList and Collection for standard images
    class var FButtonsImageCollection: TImageCollection;
    class constructor Create;
    class destructor Destroy;

    procedure UpdateButtonsImageIndex;
    procedure UpdateButtonsIcons;

    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    {$ELSE}
    procedure UpdateButtonsGlyphs;
    {$ENDIF}

    class var
    _DefaultStyleDrawType: TStyledButtonDrawType;
    _UseCustomDrawType: Boolean;
    _DefaultFamily: TStyledButtonFamily;
    _DefaultClass: TStyledButtonClass;
    _DefaultAppearance: TStyledButtonAppearance;
    _DefaultStyleRadius: Integer;
    _DefaultCursor: TCursor;

    procedure ImageListChange(Sender: TObject);
    procedure DisabledImageListChange(Sender: TObject);
    procedure ProcessButtons(AButtonProc: TButtonProc);

    function AsVCLStyle: Boolean;
    function ApplyDbnavigatorStyle: Boolean;

    procedure BtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function GetHints: TStrings;
    procedure HintsChanged(Sender: TObject);
    procedure CaptionsChanged(Sender: TObject);
    procedure InitButtons;
    procedure InitCaptions;
    procedure SetFlat(const AValue: Boolean);
    procedure SetHints(const AValue: TStrings);
    procedure SetKind(const AValue: TDBNavigatorKind);
    procedure SetSize(var W: Integer; var H: Integer);
    procedure SetVisible(const AValue: TNavButtonSet);
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    function IsCustomDrawType: Boolean;
    function IsCustomRadius: Boolean;
    function IsStoredStyleAppearance: Boolean;
    function IsStoredStyleClass: Boolean;
    function IsStoredStyleFamily: Boolean;
    procedure SetStyleAppearance(const AValue: TStyledButtonAppearance);
    procedure SetStyleClass(const AValue: TStyledButtonClass);
    procedure SetStyleDrawType(const AValue: TStyledButtonDrawType);
    procedure SetStyleFamily(const AValue: TStyledButtonFamily);
    procedure SetStyleRadius(const AValue: Integer);
    procedure SetDisabledImages(const AValue: TCustomImageList);
    procedure SetImages(const AValue: TCustomImageList);
    procedure SetStyleApplied(const AValue: Boolean);
    procedure SetShowCaptions(const AValue: Boolean);
    procedure UpdateButtons;
    function GetCaptions: TStrings;
    procedure SetCaptions(const AValue: TStrings);
    function GetAsVCLComponent: Boolean;
    procedure SetAsVCLComponent(const AValue: Boolean);
    function GetButtonItem(AIndex: TNavigateBtn): TStyledNavButton;
    procedure SetCursor(const AValue: TCursor);
    function GetCursor: TCursor;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function GetActiveStyleName: string;
    procedure ClickHandler(Sender: TObject); virtual; abstract;
    procedure InitHints; virtual;
    function GetButton(const AValue: TNavigateBtn): TStyledNavButton;
    procedure UpdateStyleElements; override;
    procedure CalcMinSize(var W, H: Integer);
    procedure CreateWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    {$IFDEF D10_4+}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ELSE}
    procedure SetButtonGlyph(Index: TNavigateBtn); virtual;
    {$ENDIF}
  public
    procedure Assign(Source: TPersistent); override;
    class procedure RegisterDefaultRenderingStyle(
      const ADrawType: TStyledButtonDrawType;
      const AFamily: TStyledButtonFamily = DEFAULT_CLASSIC_FAMILY;
      const AClass: TStyledButtonClass = DEFAULT_WINDOWS_CLASS;
      const AAppearance: TStyledButtonAppearance = DEFAULT_APPEARANCE;
      const AStyleRadius: Integer = DEFAULT_RADIUS;
      const ACursor: TCursor = DEFAULT_CURSOR); virtual;
    //Styled constructor
    constructor CreateStyled(AOwner: TComponent;
      const AFamily: TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance); virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetDbNavigatorStyle(const AStyleFamily: TStyledButtonFamily;
      const AStyleClass: TStyledButtonClass;
      const AStyleAppearance: TStyledButtonAppearance);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;

    property StyleApplied: Boolean read FStyleApplied write SetStyleApplied;
    //Access readonly properties
    property Buttons: TNavButtons read FButtons;
    property ButtonWidth: Integer read FButtonWidth;
    property ButtonHeight: Integer  read FButtonHeight;
    property VisibleButtons: TNavButtonSet read FVisibleButtons write SetVisible default NavigatorDefaultBtns;
    property MaxErrors: Integer read FMaxErrors write FMaxErrors default -1;

    property AsVCLComponent: Boolean read GetAsVCLComponent write SetAsVCLComponent stored False;
    property Align;
    property Anchors;
    property ButtonItems[Index: TNavigateBtn]: TStyledNavButton read GetButtonItem;
    property Constraints;
    property Cursor: TCursor read GetCursor write SetCursor default DEFAULT_CURSOR;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Ctl3D;
    property Captions: TStrings read GetCaptions write SetCaptions;
    property Hints: TStrings read GetHints write SetHints;
    property Kind: TDBNavigatorKind read FKind write SetKind default dbnHorizontal;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete default True;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property BeforeAction: ENavClick read FBeforeAction write FBeforeAction;
    property OnClick: ENavClick read FOnNavClick write FOnNavClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;

    //Imagelist support
    property DisabledImages: TCustomImageList read FDisabledImages write SetDisabledImages;
    property Images: TCustomImageList read FImages write SetImages;
    //new Properties
    property ShowCaptions: Boolean read FShowCaptions write SetShowCaptions default False;
    property OnEnableNavBtn:  TEnableNavBtnProc read FOnEnableNavBtn write FOnEnableNavBtn;
    //StyledComponents Attributes
    property StyleRadius: Integer read FStyleRadius write SetStyleRadius stored IsCustomRadius;
    property StyleDrawType: TStyledButtonDrawType read FStyleDrawType write SetStyleDrawType stored IsCustomDrawType;
    property StyleFamily: TStyledButtonFamily read FStyleFamily write SetStyleFamily stored IsStoredStyleFamily;
    property StyleClass: TStyledButtonClass read FStyleClass write SetStyleClass stored IsStoredStyleClass;
    property StyleAppearance: TStyledButtonAppearance read FStyleAppearance write SetStyleAppearance stored IsStoredStyleAppearance;
  end;

  { TStyledDBNavigator }
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TStyledDBNavigator = class (TCustomStyledDBNavigator)
  private
    FDataLink: TStyledNavDataLink;
    procedure SetDataSource(const AValue: TDataSource);
    function GetDataSource: TDataSource;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    procedure DataChanged;
    procedure EditingChanged;
    procedure ActiveChanged;
    procedure ClickHandler(Sender: TObject); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ApplyUpdates; virtual;
    function CanApplyUpdates: Boolean; virtual;
    procedure CancelUpdates; virtual;
    function CanCancelUpdates: Boolean; virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BtnClick(Index: TNavigateBtn); virtual;
  published
    property ActiveStyleName: string read GetActiveStyleName;
    property Align;
    property Anchors;
    property AsVCLComponent stored False;
    property BeforeAction;
    property Captions;
    property ConfirmDelete;
    property Constraints;
    property Ctl3D;
    property Cursor default DEFAULT_CURSOR;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DisabledImages;
    property Enabled;
    property Flat;
    property Hints;
    property Images;
    property Kind;
    property MaxErrors;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowCaptions;
	property OnEnableNavBtn;
    property ShowHint;
    property StyleElements;
    property TabOrder;
    property TabStop;
    property Visible;
    property VisibleButtons;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;

    //StyledComponents Attributes
    property StyleRadius;
    property StyleDrawType;
    property StyleFamily;
    property StyleClass;
    property StyleAppearance;
  end;

  { TStyledNavDataLink }

  TStyledNavDataLink = class(TDataLink)
  private
    FNavigator: TStyledDBNavigator;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(ANav: TStyledDBNavigator);
    destructor Destroy; override;
  end;

  { TStyledBindNavigator }
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TStyledBindNavigator = class(TCustomStyledDbNavigator, IBindNavigator)
  private
    FController: TBindNavigatorController;
    FBeforeAction: TNavigateButtonEvent;
    FOnNavClick: TNavigateButtonEvent;
    procedure OnActiveChanged(Sender: TObject);
    procedure OnDataChanged(Sender: TObject);
    procedure OnEditingChanged(Sender: TObject);
    function GetDataSource: TBaseLinkingBindSource;
    procedure SetDataSource(Value: TBaseLinkingBindSource);
    procedure SetVisible(const Value: TNavigateButtons);
    function GetButton(Index: TNavigateButton): TStyledNavButton;
    function GetOrientation: TNavigatorOrientation;
    procedure SetOrientation(const Value: TNavigatorOrientation);
    function NavigateButtonToNavBtn(const AValue: TNavigateButton): TNavigateBtn;
    function NavBtnToNavigateButton(const AValue: TNavigateBtn): TNavigateButton;
    function NavigateButtonsToNavBtns(const AValue: TNavigateButtons): TNavButtonSet;
    function NavBtnsToNavigateButtons(const AValue: TNavButtonSet): TNavigateButtons;
    function GetVisibleButtons: TNavigateButtons;
  protected
    procedure ClickHandler(Sender: TObject); override;
    property Buttons[Index: TNavigateButton]: TStyledNavButton read GetButton;
    procedure ActiveChanged;
    procedure DataChanged;
    procedure EditingChanged;
    //procedure BtnIDClick(Index: TNavBtnID); override;
  public
    procedure BtnClick(Index: TNavigateButton); reintroduce; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property AsVCLComponent stored False;
    property BeforeAction: TNavigateButtonEvent read FBeforeAction write FBeforeAction;
    property Captions;
    property ConfirmDelete;
    property Constraints;
    property Ctl3D;
    property Cursor default DEFAULT_CURSOR;
    property DataSource: TBaseLinkingBindSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DisabledImages;
    property Enabled;
    property Flat;
    property Hints;
    property Images;
    property Kind;
    property MaxErrors;
    property Orientation: TNavigatorOrientation read GetOrientation write SetOrientation default orHorizontal;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowCaptions;
	property OnEnableNavBtn;
    property ShowHint;
    property StyleElements;
    property TabOrder;
    property TabStop;
    property Visible;
    property VisibleButtons: TNavigateButtons read GetVisibleButtons write SetVisible default NavigatorDefaultButtons;

    property OnClick: TNavigateButtonEvent read FOnNavClick write FOnNavClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;

    //StyledComponents Attributes
    property StyleRadius;
    property StyleDrawType;
    property StyleFamily;
    property StyleClass;
    property StyleAppearance;
  end;

implementation

uses
  Vcl.Consts
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.Graphics
  , System.Types
  , System.RTLConsts
  , Vcl.StyledCmpMessages
  , Vcl.StyledTaskDialog
  ;

const
  DEFAULT_BTN_IMAGE_SIZE = 15;
  COLORED_IMAGE_COLLECTION = 'STYLED_NB_';
  WHITE_IMAGE_COLLECTION = 'STYLED_NAVW_';
  BLACK_IMAGE_COLLECTION = 'STYLED_NAVB_';
  VERTICAL_ICON = '_VERT';
  DEFAULT_IMAGE_MARGIN = 4;

var
  BtnTypeName: array[TNavigateBtn] of PChar = ('FIRST', 'PRIOR', 'NEXT',
    'LAST', 'INSERT', 'DELETE', 'EDIT', 'POST', 'CANCEL', 'REFRESH', 'APPLYUPDATES',
    'CANCELUPDATES');
  BtnHintId: array[TNavigateBtn] of Pointer = (@SFirstRecord, @SPriorRecord,
    @SNextRecord, @SLastRecord, @SInsertRecord, @SDeleteRecord, @SEditRecord,
    @SPostEdit, @SCancelEdit, @SRefreshRecord, @SApplyUpdates, @SCancelUpdates);
  BtnCaptionId: array[TNavigateBtn] of Pointer = (@CaptionFirstRecord, @CaptionPriorRecord,
    @CaptionNextRecord, @CaptionLastRecord, @CaptionInsertRecord, @CaptionDeleteRecord, @CaptionEditRecord,
    @CaptionPostEdit, @CaptionCancelEdit, @CaptionRefreshRecord, @CaptionApplyUpdates, @CaptionCancelUpdates);

{$IFDEF D10_4+}
class constructor TCustomStyledDBNavigator.Create;
begin
end;

procedure InitButtonsImageCollection;
var
  LBtnName: string;

  procedure AddImageToCollection(const APrefix, ABtnName: string);
  begin
    TCustomStyledDBNavigator.FButtonsImageCollection.Add(
      APrefix+ABtnName, HInstance,
      APrefix+ABtnName, ['', '_20X']);
  end;

begin
  if TCustomStyledDBNavigator.FButtonsImageCollection <> nil then
    Exit;
  TCustomStyledDBNavigator.FButtonsImageCollection := TImageCollection.Create(nil);
  for var I := Low(BtnTypeName) to High(BtnTypeName) do
  begin
    LBtnName := BtnTypeName[I];
    //Add colored image (prefix SNB in StyledNavButtonsPNG.RES)
    AddImageToCollection(COLORED_IMAGE_COLLECTION,LBtnName);
    //Add black image (prefix NAVB in StyledNavButtonsPNG.RES)
    AddImageToCollection(BLACK_IMAGE_COLLECTION,LBtnName);
    //Add white image (prefix NAVW in StyledNavButtonsPNG.RES)
    AddImageToCollection(WHITE_IMAGE_COLLECTION,LBtnName);
  end;
  //Add also vertical images
  for var I := TNavigateBtn.nbFirst to TNavigateBtn.nbLast do
  begin
    LBtnName := BtnTypeName[I] + VERTICAL_ICON;
    //Add colored image (prefix SNB in StyledNavButtonsPNG.RES)
    AddImageToCollection(COLORED_IMAGE_COLLECTION,LBtnName);
    //Add black image (prefix NAVB in StyledNavButtonsPNG.RES)
    AddImageToCollection(BLACK_IMAGE_COLLECTION,LBtnName);
    //Add white image (prefix NAVW in StyledNavButtonsPNG.RES)
    AddImageToCollection(WHITE_IMAGE_COLLECTION,LBtnName);
  end;
end;
{$ENDIF}

constructor TCustomStyledDBNavigator.Create(AOwner: TComponent);
begin
  CreateStyled(AOwner,
    _DefaultFamily,
    _DefaultClass,
    _DefaultAppearance);
end;

constructor TCustomStyledDBNavigator.CreateStyled(AOwner: TComponent;
  const AFamily: TStyledButtonFamily; const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance);
begin
  {$IFDEF D10_4+}
  InitButtonsImageCollection;
  {$ENDIF}
  inherited Create(AOwner);
  FShowCaptions := False;
  ControlStyle := ControlStyle
    - [csDoubleClicks, csAcceptsControls, csSetCaption, csGestures] + [csOpaque];
  if not NewStyleControls then
    ControlStyle := ControlStyle + [csFramed];
  FVisibleButtons := NavigatorDefaultBtns;
  FHints := TStringList.Create;
  FCaptions := TStringList.Create;
  TStringList(FHints).OnChange := HintsChanged;
  TStringList(FCaptions).OnChange := CaptionsChanged;
  {$IFDEF D10_4+}
  FButtonImages := TVirtualImageList.Create(nil);
  FButtonImages.AutoFill := True;
  FButtonImages.ImageCollection := FButtonsImageCollection;
  FButtonImages.SetSize(DEFAULT_BTN_IMAGE_SIZE, DEFAULT_BTN_IMAGE_SIZE);
  {$ENDIF}

  FStyleDrawType := _DefaultStyleDrawType;
  FStyleRadius := _DefaultStyleRadius;
  FStyleFamily := AFamily;
  FStyleClass := AClass;
  FStyleAppearance := AAppearance;
  Cursor := _DefaultCursor;

  InitButtons;
  InitHints;
  InitCaptions;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  MaxErrors := -1;
  Kind := dbnHorizontal;
  Width := 241;
  Height := 25;
  FButtonWidth := 0;
  FButtonHeight := 0;
  FocusedButton := TNavigateBtn.nbFirst;
  FConfirmDelete := True;
  FullRepaint := False;

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;

  FDisabledImageChangeLink := TChangeLink.Create;
  FDisabledImageChangeLink.OnChange := DisabledImageListChange;

  //FHotImageChangeLink := TChangeLink.Create;
  //FHotImageChangeLink.OnChange := HotImageListChange;

  ParentBackground := True;
  ParentColor := True;
  BevelOuter := bvNone;
  Flat := False;
end;

{$IFNDEF D10_4+}
procedure TCustomStyledDBNavigator.UpdateButtonsGlyphs;
var
  I: TNavigateBtn;
  UseGlyphs: Boolean;
begin
  UseGlyphs := not Assigned(FImages); //not StyleServices.Enabled or StyleServices.IsSystemStyle;
  for I := Low(FButtons) to High(FButtons) do
  begin
    if UseGlyphs and (Buttons[I].ImageIndex = -1) then
      SetButtonGlyph(I)
    else if not UseGlyphs then
      Buttons[I].ImageIndex := Ord(I);
  end;
end;
{$ENDIF}

procedure TCustomStyledDBNavigator.CreateWnd;
begin
  inherited;
  {$IFNDEF D10_4+}
  UpdateButtonsGlyphs;
  {$ENDIF}
end;

destructor TCustomStyledDBNavigator.Destroy;
begin
  FreeAndNil(FDefaultHints);
  FreeAndNil(FDefaultCaptions);
  FreeAndNil(FHints);
  FreeAndNil(FCaptions);
  FreeAndNil(FDisabledImageChangeLink);
  FreeAndNil(FImageChangeLink);
  {$IFDEF D10_4+}
  FreeAndNil(FButtonImages);
  {$ENDIF}
  inherited Destroy;
end;

{$IFDEF D10_4+}
class destructor TCustomStyledDBNavigator.Destroy;
begin
  FreeAndNil(FButtonsImageCollection);
end;

procedure TCustomStyledDBNavigator.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  if FButtonImages <> nil then
    FButtonImages.SetSize(MulDiv(FButtonImages.Width, M, D), MulDiv(FButtonImages.Height, M, D));
end;

procedure TCustomStyledDBNavigator.CMStyleChanged(var Message: TMessage);
begin
  UpdateButtonsIcons;
  Invalidate;
end;

procedure TCustomStyledDBNavigator.UpdateButtonsImageIndex;
var
  I: TNavigateBtn;
  Btn: TStyledNavButton;
begin
  for I := Low(FButtons) to High(FButtons) do
  begin
    Btn := Buttons[I];
    if (FKind = dbnVertical) and (I in NavigatorMoveBtns) then
      Btn.ImageIndex := Ord(I) + Ord(High(FButtons)) +1
    else
      Btn.ImageIndex := Ord(I);
  end;
end;

procedure TCustomStyledDBNavigator.UpdateButtonsIcons;
var
  LStyleName, LBtnName: string;
  LThemeAttribute: TThemeAttribute;
begin
  if Assigned(FImages) then
  begin
    UpdateButtonsImageIndex;
    Exit;
  end;

  if (StyleFamily = DEFAULT_CLASSIC_FAMILY) then
    LStyleName := StyleClass;
  if LStyleName = 'Windows' then
    LStyleName := GetActiveStyleName;
  for var I := Low(FButtons) to High(FButtons) do
  begin
    var Btn := Buttons[I];
    LBtnName := BtnTypeName[I];

    if (LStyleName = 'Windows') then
    begin
      //Use colored images (for backward compatibility)
      LBtnName := COLORED_IMAGE_COLLECTION+LBtnName;
    end
    else
    begin
      if GetStyleAttributes(LStyleName, LThemeAttribute) then
      begin
        if LThemeAttribute.ThemeType = ttLight then
          //Use black images for light theme
          LBtnName := BLACK_IMAGE_COLLECTION+LBtnName
        else
          //Use white images for dark theme
          LBtnName := WHITE_IMAGE_COLLECTION+LBtnName;
      end
      else
        //Use colored images (for backward compatibility)
        LBtnName := COLORED_IMAGE_COLLECTION+LBtnName;
    end;

    //Vertical images
    if (FKind = dbnVertical) and
      (I in NavigatorMoveBtns) then
      LBtnName := LBtnName+VERTICAL_ICON;
    Btn.ImageName := LBtnName;
  end;
end;
{$ELSE}
procedure TCustomStyledDBNavigator.SetButtonGlyph(Index: TNavigateBtn);
var
  LResName: string;
begin
  FmtStr(LResName, COLORED_IMAGE_COLLECTION+'%s', [BtnTypeName[Index]]);
  FButtons[Index].NumGlyphs := 2;
  FButtons[Index].Glyph.LoadFromResourceName(HInstance, LResName);
end;
{$ENDIF}

procedure TCustomStyledDBNavigator.Paint;
begin
  if StyleServices.Enabled and not StyleServices.IsSystemStyle  then
    with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := StyleServices.GetSystemColor(clBtnFace);
      FillRect(Rect(0, 0, Width, Height));
    end
  else
    inherited;
end;

procedure TCustomStyledDBNavigator.ImageListChange(Sender: TObject);
begin
  ProcessButtons(
    procedure (ABtn: TStyledNavButton)
    begin
      ABtn.Images := Images;
      if not Assigned(Images) then
      {$IFDEF D10_4+}
        ABtn.Images := FButtonImages;
      {$ELSE}
        ABtn.ImageIndex := -1;
      {$ENDIF}
    end);
  {$IFDEF D10_4+}
  UpdateButtonsIcons;
  {$ELSE}
  UpdateButtonsGlyphs;
  {$ENDIF}
end;

procedure TCustomStyledDBNavigator.DisabledImageListChange(Sender: TObject);
begin
  ProcessButtons(
    procedure (ABtn: TStyledNavButton)
    begin
      ABtn.DisabledImages := DisabledImages;
    end
  );
end;

procedure TCustomStyledDBNavigator.ProcessButtons(AButtonProc: TButtonProc);
var
  I: TNavigateBtn;
begin
  for I := Low(FButtons) to High(FButtons) do
  begin
    if Assigned(FButtons[I]) then
      AButtonProc(FButtons[I]);
  end;
end;

class procedure TCustomStyledDBNavigator.RegisterDefaultRenderingStyle(
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

procedure TCustomStyledDBNavigator.InitButtons;
var
  I: TNavigateBtn;
  Btn: TStyledNavButton;
  X, Y: Integer;
begin
  FMinBtnSize := Point(20, 18);
  X := 0;
  Y := 0;
  for I := Low(FButtons) to High(FButtons) do
  begin
    Btn := TStyledNavButton.Create(Self);
    Btn.Flat := Flat;
    Btn.Index := I;
    Btn.Visible := I in FVisibleButtons;
    Btn.EnabledNavBtn(True);
    Btn.SetBounds (X, Y, FMinBtnSize.X, FMinBtnSize.Y);
    Btn.OnClick := ClickHandler;
    Btn.OnMouseDown := BtnMouseDown;
    Btn.Parent := Self;
    Btn.Cursor := Cursor;
    FButtons[I] := Btn;
    if Kind = dbnHorizontal then
      X := X + FMinBtnSize.X
    else
      Y := Y + FMinBtnSize.Y;
  end;
  FButtons[TNavigateBtn.nbPrior].NavStyle := FButtons[TNavigateBtn.nbPrior].NavStyle + [nsAllowTimer];
  FButtons[TNavigateBtn.nbNext].NavStyle  := FButtons[TNavigateBtn.nbNext].NavStyle + [nsAllowTimer];
  {$IFDEF D10_4+}
  UpdateButtonsIcons;
  {$ELSE}
  UpdateButtonsGlyphs;
  {$ENDIF}
end;

procedure TCustomStyledDBNavigator.InitCaptions;
var
  I: Integer;
  J: TNavigateBtn;
begin
  if not Assigned(FDefaultCaptions) then
  begin
    FDefaultCaptions := TStringList.Create;
    for J := Low(FButtons) to High(FButtons) do
      FDefaultCaptions.Add(LoadResString(BtnCaptionId[J]));
  end;
  for J := Low(FButtons) to High(FButtons) do
    FButtons[J].Caption := FDefaultCaptions[Ord(J)];
  J := Low(FButtons);
  for I := 0 to (FCaptions.Count - 1) do
  begin
    if FCaptions.Strings[I] <> '' then
      FButtons[J].Caption := FCaptions.Strings[I];
    if J = High(FButtons) then
      Exit;
    Inc(J);
  end;
end;

procedure TCustomStyledDBNavigator.InitHints;
var
  I: Integer;
  J: TNavigateBtn;
begin
  if not Assigned(FDefaultHints) then
  begin
    FDefaultHints := TStringList.Create;
    for J := Low(FButtons) to High(FButtons) do
      FDefaultHints.Add(LoadResString(BtnHintId[J]));
  end;
  for J := Low(FButtons) to High(FButtons) do
    FButtons[J].Hint := FDefaultHints[Ord(J)];
  J := Low(FButtons);
  for I := 0 to (FHints.Count - 1) do
  begin
    if FHints.Strings[I] <> '' then
      FButtons[J].Hint := FHints.Strings[I];
    if J = High(FButtons) then
      Exit;
    Inc(J);
  end;
end;

function TCustomStyledDBNavigator.IsCustomDrawType: Boolean;
begin
  Result := FCustomDrawType;
end;

function TCustomStyledDBNavigator.IsCustomRadius: Boolean;
begin
  Result := StyleRadius <> DEFAULT_RADIUS;
end;

function TCustomStyledDBNavigator.IsStoredStyleAppearance: Boolean;
var
  LClass: TStyledButtonClass;
  LAppearance: TStyledButtonAppearance;
  LButtonFamily: TButtonFamily;
begin
  StyleFamilyCheckAttributes(FStyleFamily, LClass, LAppearance, LButtonFamily);
  Result := FStyleAppearance <> LAppearance;
end;

function TCustomStyledDBNavigator.IsStoredStyleClass: Boolean;
var
  LClass: TStyledButtonClass;
  LAppearance: TStyledButtonAppearance;
  LButtonFamily: TButtonFamily;
begin
  StyleFamilyCheckAttributes(FStyleFamily, LClass, LAppearance, LButtonFamily);

  if AsVCLStyle then
  begin
    Result := (FStyleClass <> GetActiveStyleName)
      and not SameText(FStyleClass, 'Windows');
  end
  else
    Result := FStyleClass <> LClass;
end;

function TCustomStyledDBNavigator.IsStoredStyleFamily: Boolean;
begin
  Result := FStyleFamily <> DEFAULT_CLASSIC_FAMILY;
end;

procedure TCustomStyledDBNavigator.HintsChanged(Sender: TObject);
begin
  InitHints;
end;

procedure TCustomStyledDBNavigator.CaptionsChanged(Sender: TObject);
begin
  InitCaptions;
end;

procedure TCustomStyledDBNavigator.SetFlat(const AValue: Boolean);
begin
  if FFlat <> AValue then
  begin
    FFlat := AValue;
    ProcessButtons(
      procedure (ABtn: TStyledNavButton)
      begin
        ABtn.Flat := AValue;
      end);
  end;
end;

procedure TCustomStyledDBNavigator.SetHints(const AValue: TStrings);
begin
  if AValue.Text = FDefaultHints.Text then
    FHints.Clear else
    FHints.Assign(AValue);
end;

procedure TCustomStyledDBNavigator.SetCaptions(const AValue: TStrings);
begin
  if AValue.Text = FDefaultCaptions.Text then
    FCaptions.Clear else
    FCaptions.Assign(AValue);
end;

procedure TCustomStyledDBNavigator.SetCursor(const AValue: TCursor);
begin
  if AValue <> Cursor then
  begin
    inherited Cursor := AValue;
    if not FNoCopyCursor then
      ProcessButtons(
        procedure (ABtn: TStyledNavButton)
        begin
          ABtn.Cursor := AValue;
        end
      );
  end;
end;

procedure TCustomStyledDBNavigator.SetImages(const AValue: TCustomImageList);
begin
  if FImages <> AValue then
  begin
    FImages := AValue;
    ImageListChange(Self);
  end;
end;

function TCustomStyledDBNavigator.GetHints: TStrings;
begin
  if (csDesigning in ComponentState) and not (csWriting in ComponentState) and
     not (csReading in ComponentState) and (FHints.Count = 0) then
    Result := FDefaultHints else
    Result := FHints;
end;

function TCustomStyledDBNavigator.GetCaptions: TStrings;
begin
  if (csDesigning in ComponentState) and not (csWriting in ComponentState) and
     not (csReading in ComponentState) and (FCaptions.Count = 0) then
    Result := FDefaultCaptions else
    Result := FCaptions;
end;

procedure TCustomStyledDBNavigator.SetKind(const AValue: TDBNavigatorKind);
begin
  if FKind <> AValue then
  begin
    FKind := AValue;
    if not (csLoading in ComponentState) then
      SetBounds(Left, Top, Height, Width);
    {$IFDEF D10_4+}
    UpdateButtonsIcons;
    {$ELSE}
    UpdateButtonsGlyphs;
    {$ENDIF}
    Invalidate;
  end;
end;

function TCustomStyledDBNavigator.GetActiveStyleName: string;
begin
  Result := Vcl.ButtonStylesAttributes.GetActiveStyleName(Self);
end;

function TCustomStyledDBNavigator.GetButton(
  const AValue: TNavigateBtn): TStyledNavButton;
begin
  Result := FButtons[Avalue];
end;

function TCustomStyledDBNavigator.GetButtonItem(
  AIndex: TNavigateBtn): TStyledNavButton;
begin
  Result := GetButton(AIndex);
end;

procedure TCustomStyledDBNavigator.GetChildren(Proc: TGetChildProc; Root: TComponent);
//var
//  J: TNavigateBtn;
begin
//  for J := Low(FButtons) to High(FButtons) do
//    Proc(FButtons[J]);
end;

function TCustomStyledDBNavigator.GetCursor: TCursor;
begin
  Result := inherited Cursor;
end;

procedure TCustomStyledDBNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent = Images) then
      Images := {$IFDEF D10_4+}FButtonImages;{$ELSE}nil;{$ENDIF}
    if (AComponent = DisabledImages) then
      DisabledImages := nil;
  end;
end;

procedure TCustomStyledDBNavigator.SetVisible(const AValue: TNavButtonSet);
var
  I: TNavigateBtn;
  W, H: Integer;
begin
  W := Width;
  H := Height;
  FVisibleButtons := AValue;
  for I := Low(FButtons) to High(FButtons) do
    FButtons[I].Visible := I in FVisibleButtons;
  SetSize(W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
  Invalidate;
end;

procedure TCustomStyledDBNavigator.UpdateStyleElements;
var
  LStyleClass: TStyledButtonClass;
begin
  if AsVCLStyle then
  begin
    //if StyleElements contains seClient then Update style
    //as VCL Style assigned to Dbnavigator or Global VCL Style
    if seBorder in StyleElements then
      StyleAppearance := DEFAULT_APPEARANCE;
    LStyleClass := GetActiveStyleName;
    FStyleClass := LStyleClass;
    StyleApplied := ApplyDbnavigatorStyle;
  ProcessButtons(
    procedure (ABtn: TStyledNavButton)
    begin
      ABtn.UpdateStyleElements;
      ABtn.StyleDrawType := Self.StyleDrawType;
    end);
  end;
  inherited;
end;

procedure TCustomStyledDBNavigator.CalcMinSize(var W, H: Integer);
var
  Count: Integer;
  I: TNavigateBtn;
begin
  if (csLoading in ComponentState) then Exit;
  if FButtons[TNavigateBtn.nbFirst] = nil then Exit;

  Count := 0;
  for I := Low(FButtons) to High(FButtons) do
    if FButtons[I].Visible then
      Inc(Count);
  if Count = 0 then Inc(Count);
  {$IFDEF D10_4+}
  if Kind = dbnHorizontal then
  begin
    W := Max(W, Count * ScaleValue(FMinBtnSize.X));
    H := Max(H, ScaleValue(FMinBtnSize.Y));
    if Align = alNone then W := (W div Count) * Count;
  end
  else
  begin
    W := Max(W, ScaleValue(FMinBtnSize.X));
    H := Max(H, Count * ScaleValue(FMinBtnSize.Y));
    if Align = alNone then H := (H div Count) * Count;
  end;
  {$ELSE}
  if Kind = dbnHorizontal then
  begin
    W := Max(W, Count * FMinBtnSize.X);
    H := Max(H, FMinBtnSize.Y);
    if Align = alNone then W := (W div Count) * Count;
  end
  else
  begin
    W := Max(W, FMinBtnSize.X);
    H := Max(H, Count * FMinBtnSize.Y);
    if Align = alNone then H := (H div Count) * Count;
  end;
  {$ENDIF}
end;

procedure TCustomStyledDBNavigator.UpdateButtons;
begin
  ProcessButtons(
    procedure (ABtn: TStyledNavButton)
    begin
      ABtn.UpdateButtonContent;
    end);
end;

procedure TCustomStyledDBNavigator.SetShowCaptions(const AValue: Boolean);
begin
  if FShowCaptions <> AValue then
  begin
    FShowCaptions := AValue;
    UpdateButtons;
  end;
end;

procedure TCustomStyledDBNavigator.SetSize(var W: Integer; var H: Integer);
var
  Count: Integer;
  I: TNavigateBtn;
  Space, Temp, Remain: Integer;
  X, Y: Integer;
begin
  if (csLoading in ComponentState) then
    Exit;
  if FButtons[TNavigateBtn.nbFirst] = nil then
    Exit;

  CalcMinSize(W, H);

  Count := 0;
  for I := Low(FButtons) to High(FButtons) do
    if FButtons[I].Visible then
      Inc(Count);
  if Count = 0 then Inc(Count);

  if Kind = dbnHorizontal then
  begin
    FButtonWidth := W div Count;
    FButtonHeight := H;
    Temp := Count * FButtonWidth;
    if Align = alNone then W := Temp;
    Remain := W - Temp;
  end
  else
  begin
    FButtonWidth := W;
    FButtonHeight := H div Count;
    Temp := Count * FButtonHeight;
    if Align = alNone then H := Temp;
    Remain := H - Temp;
  end;

  X := 0;
  Y := 0;
  Temp := Count div 2;

  for I := Low(FButtons) to High(FButtons) do
  begin
    if FButtons[I].Visible then
    begin
      Space := 0;
      if Remain <> 0 then
      begin
        Dec(Temp, Remain);
        if Temp < 0 then
        begin
          Inc(Temp, Count);
          Space := 1;
        end;
      end;
      if Kind = dbnHorizontal then
      begin
        FButtons[I].SetBounds(X, Y, FButtonWidth + Space, Height);
        Inc(X, FButtonWidth + Space);
      end
      else
      begin
        FButtons[I].SetBounds(X, Y, FButtonWidth, FButtonHeight + Space);
        Inc(Y, FButtonHeight + Space);
      end;
    end
    else
      if Kind = dbnHorizontal then
        FButtons[I].SetBounds(Width + 1, 0, FButtonWidth, Height)
      else
        FButtons[I].SetBounds(0, Height + 1, FButtonWidth, FButtonHeight);
  end;
end;

procedure TCustomStyledDBNavigator.SetStyleAppearance(
  const AValue: TStyledButtonAppearance);
var
  LValue: TStyledButtonAppearance;
begin
  LValue := AValue;
  if LValue = '' then
    LValue := DEFAULT_APPEARANCE;
  if (FStyleAppearance <> LValue) or not FStyleApplied then
  begin
    ProcessButtons(
      procedure (ABtn: TStyledNavButton)
      begin
        if ABtn.StyleAppearance = StyleAppearance then
          ABtn.StyleAppearance := LValue;
      end);
    FStyleAppearance := LValue;
    StyleApplied := ApplyDbnavigatorStyle;
  end;
end;

procedure TCustomStyledDBNavigator.SetStyleApplied(const AValue: Boolean);
begin
  FStyleApplied := AValue;
end;

procedure TCustomStyledDBNavigator.SetStyleClass(const AValue: TStyledButtonClass);
var
  LValue: TStyledButtonClass;
begin
  LValue := AValue;
  if LValue = '' then
    LValue := DEFAULT_WINDOWS_CLASS;
  if (LValue <> Self.FStyleClass) or not FStyleApplied then
  begin
    ProcessButtons(
      procedure (ABtn: TStyledNavButton)
      begin
        ABtn.StyleClass := LValue;
      end);
    FStyleClass := LValue;
    {$IFDEF D10_4+}
    UpdateButtonsIcons;
    {$ENDIF}
    StyleApplied := ApplyDbnavigatorStyle;
    if (FStyleFamily = DEFAULT_CLASSIC_FAMILY) and
      (LValue <> 'Windows') then
      StyleElements := [seFont, seBorder];
  end;
end;

procedure TCustomStyledDBNavigator.SetStyleDrawType(
  const AValue: TStyledButtonDrawType);
begin
  FCustomDrawType := True;
  ProcessButtons(
    procedure (ABtn: TStyledNavButton)
    begin
      if ABtn.StyleDrawType = StyleDrawType then
        ABtn.StyleDrawType := AValue;
    end);
  if FStyleDrawType <> AValue then
  begin
    FStyleDrawType := AValue;
    StyleApplied := ApplyDbnavigatorStyle;
  end;
end;

procedure TCustomStyledDBNavigator.SetStyleFamily(const AValue: TStyledButtonFamily);
var
  LValue: TStyledButtonFamily;
begin
  LValue := AValue;
  if LValue = '' then
    LValue := DEFAULT_CLASSIC_FAMILY;
  if (LValue <> Self.FStyleFamily) or not FStyleApplied then
  begin
    ProcessButtons(
      procedure (ABtn: TStyledNavButton)
      begin
        if ABtn.StyleFamily = FStyleFamily then
          ABtn.StyleFamily := LValue;
      end);
    FStyleFamily := LValue;
    StyleApplied := ApplyDbnavigatorStyle;
    {$IFDEF D10_4+}
    UpdateButtonsIcons;
    {$ENDIF}
  end;
  if FStyleFamily = DEFAULT_CLASSIC_FAMILY then
    StyleElements := [seFont, seClient, seBorder];
end;

procedure TCustomStyledDBNavigator.SetStyleRadius(const AValue: Integer);
begin
  if FStyleRadius <> AValue then
  begin
    if AValue <= 0 then
      raise EReadError.create(SInvalidProperty);
    ProcessButtons(
      procedure (ABtn: TStyledNavButton)
      begin
        if ABtn.StyleRadius = FStyleRadius then
          ABtn.StyleRadius := AValue;
      end);
    FStyleRadius := AValue;
    StyleApplied := ApplyDbnavigatorStyle;
  end;
end;

procedure TCustomStyledDBNavigator.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  if not HandleAllocated then SetSize(W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end;

procedure TCustomStyledDBNavigator.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;
  W := Width;
  H := Height;
  SetSize(W, H);
end;

procedure TCustomStyledDBNavigator.WMWindowPosChanging(var Message: TWMWindowPosChanging);
begin
  inherited;
  if (SWP_NOSIZE and Message.WindowPos.Flags) = 0 then
    CalcMinSize(Message.WindowPos.cx, Message.WindowPos.cy);
end;

procedure TCustomStyledDBNavigator.BtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OldFocus: TNavigateBtn;
begin
  OldFocus := FocusedButton;
  FocusedButton := TStyledNavButton (Sender).Index;
  if TabStop and (GetFocus <> Handle) and CanFocus then
  begin
    SetFocus;
    if (GetFocus <> Handle) then
      Exit;
  end
  else if TabStop and (GetFocus = Handle) and (OldFocus <> FocusedButton) then
  begin
    FButtons[OldFocus].Invalidate;
    FButtons[FocusedButton].Invalidate;
  end;
end;

function TCustomStyledDBNavigator.ApplyDbnavigatorStyle: Boolean;
var
  LButtonFamily: TButtonFamily;
  LAttributesNormal, LAttributesOther: TStyledButtonAttributes;
begin
  Result := StyleFamilyCheckAttributes(FStyleFamily,
    FStyleClass, FStyleAppearance, LButtonFamily);
  if Result or (csDesigning in ComponentState) then
  begin
    LAttributesNormal := nil;
    LAttributesOther := nil;
    try
      LAttributesNormal := TStyledButtonAttributes.Create(nil);
      LAttributesOther := TStyledButtonAttributes.Create(nil);
      StyleFamilyUpdateAttributes(
        FStyleFamily,
        FStyleClass,
        FstyleAppearance,
        LAttributesNormal,
        LAttributesOther,
        LAttributesOther,
        LAttributesOther,
        LAttributesOther);
(*
      if not FCustomDrawType then
      begin
        FStyleDrawType := LAttributesNormal.DrawType;
        FCustomDrawType := False;
      end;
*)
    finally
      LAttributesNormal.Free;
      LAttributesOther.Free;
    end;
  end;
end;

procedure TCustomStyledDBNavigator.Assign(Source: TPersistent);
var
  LNavigator: TCustomStyledDBNavigator;
begin
  inherited Assign(Source);
  if Source is TCustomStyledDBNavigator then
  begin
    LNavigator := TCustomStyledDBNavigator(Source);
    FFlat := LNavigator.FFlat;
    FKind := LNavigator.FKind;
    FConfirmDelete := LNavigator.FConfirmDelete;
    DisabledImages := LNavigator.FDisabledImages;
    Images :=  LNavigator.FImages;
    FShowCaptions := LNavigator.FShowCaptions;
    FStyleRadius := LNavigator.FStyleRadius;
    FStyleDrawType := LNavigator.FStyleDrawType;
    FStyleFamily := LNavigator.FStyleFamily;
    FStyleClass := LNavigator.FStyleClass;
    FStyleAppearance := LNavigator.FStyleAppearance;
    Invalidate;
  end;
end;

function TCustomStyledDBNavigator.AsVCLStyle: Boolean;
begin
  Result := (StyleFamily = DEFAULT_CLASSIC_FAMILY) and
    (seClient in StyleElements);
end;

procedure TCustomStyledDBNavigator.SetAsVCLComponent(const AValue: Boolean);
begin
  if AValue <> GetAsVCLComponent then
  begin
    if AValue then
    begin
      FStyleFamily := DEFAULT_CLASSIC_FAMILY;
      FStyleClass := DEFAULT_WINDOWS_CLASS;
      FStyleAppearance := DEFAULT_APPEARANCE;
      StyleElements := StyleElements + [seClient];
      FCustomDrawType := False;
    end
    else if FStyleFamily = DEFAULT_CLASSIC_FAMILY then
    begin
      StyleElements := StyleElements - [seClient];
    end;
    UpdateStyleElements;
  end;
end;

function TCustomStyledDBNavigator.GetAsVCLComponent: Boolean;
begin
  Result := (StyleFamily = DEFAULT_CLASSIC_FAMILY) and
    (seClient in StyleElements)  and
    (FStyleClass = GetActiveStyleName);
end;

procedure TCustomStyledDBNavigator.WMSetFocus(var Message: TWMSetFocus);
begin
  FButtons[FocusedButton].Invalidate;
end;

procedure TCustomStyledDBNavigator.WMKillFocus(var Message: TWMKillFocus);
begin
  FButtons[FocusedButton].Invalidate;
end;

procedure TCustomStyledDBNavigator.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewFocus: TNavigateBtn;
  OldFocus: TNavigateBtn;
begin
  OldFocus := FocusedButton;
  case Key of
    VK_RIGHT:
      begin
        if OldFocus < High(FButtons) then
        begin
          NewFocus := OldFocus;
          repeat
            NewFocus := Succ(NewFocus);
          until (NewFocus = High(FButtons)) or (FButtons[NewFocus].Visible);
          if FButtons[NewFocus].Visible then
          begin
            FocusedButton := NewFocus;
            FButtons[OldFocus].Invalidate;
            FButtons[NewFocus].Invalidate;
          end;
        end;
      end;
    VK_LEFT:
      begin
        NewFocus := FocusedButton;
        repeat
          if NewFocus > Low(FButtons) then
            NewFocus := Pred(NewFocus);
        until (NewFocus = Low(FButtons)) or (FButtons[NewFocus].Visible);
        if NewFocus <> FocusedButton then
        begin
          FocusedButton := NewFocus;
          FButtons[OldFocus].Invalidate;
          FButtons[FocusedButton].Invalidate;
        end;
      end;
    VK_SPACE:
      begin
        if FButtons[FocusedButton].Enabled then
          FButtons[FocusedButton].Click;
      end;
  end;
end;

procedure TCustomStyledDBNavigator.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TCustomStyledDBNavigator.SetDbNavigatorStyle(
  const AStyleFamily: TStyledButtonFamily;
  const AStyleClass: TStyledButtonClass;
  const AStyleAppearance: TStyledButtonAppearance);
begin
  StyleFamily := AStyleFamily;
  StyleClass := AStyleClass;
  StyleAppearance := AStyleAppearance;
  if not ApplyDbnavigatorStyle then
    raise EStyledButtonError.CreateFmt(ERROR_SETTING_DBNAVIGATOR_STYLE,
      [AStyleFamily, AStyleClass, AStyleAppearance]);
end;

procedure TCustomStyledDBNavigator.SetDisabledImages(const AValue: TCustomImageList);
begin
  if FDisabledImages <> AValue then
  begin
    FDisabledImages := AValue;
    DisabledImageListChange(Self);
  end;
end;

procedure TCustomStyledDBNavigator.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  SetSize(W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
  InitHints;
end;

procedure TCustomStyledDBNavigator.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  LControl: TControl;
begin
  inherited;
  LControl := ControlAtPos(TPoint.Create(X,Y), True, True);
  if (LControl is TStyledNavButton) then
  begin
    FNoCopyCursor := True;
    try
      if not LControl.Enabled then
        Cursor := crDefault
      else
        Cursor := LControl.Cursor;
    finally
      FNoCopyCursor := False;
    end;
  end;
end;

{ TStyledDBNavigator }

procedure TStyledDBNavigator.ActiveChanged;
var
  I: TNavigateBtn;
begin
  if not (Enabled and FDataLink.Active) then
    for I := Low(FButtons) to High(FButtons) do
      FButtons[I].EnabledNavBtn(False)
  else
  begin
    DataChanged;
    EditingChanged;
  end;
end;

procedure TStyledDBNavigator.ApplyUpdates;
var
  Intf: IDataSetCommandSupport;
begin
  if (Self.DataSource <> nil) and Supports(Self.DataSource.DataSet, IDataSetCommandSupport, Intf) then
    Intf.ExecuteCommand(sApplyUpdatesDataSetCommand, [MaxErrors])
end;

procedure TStyledDBNavigator.BtnClick(Index: TNavigateBtn);
begin
  if (DataSource <> nil) and (DataSource.State <> dsInactive) then
  begin
    if not (csDesigning in ComponentState) and Assigned(FBeforeAction) then
      FBeforeAction(Self, Index);
    with DataSource.DataSet do
    begin
      case Index of
        TNavigateBtn.nbPrior: Prior;
        TNavigateBtn.nbNext: Next;
        TNavigateBtn.nbFirst: First;
        TNavigateBtn.nbLast: Last;
        TNavigateBtn.nbInsert: Insert;
        TNavigateBtn.nbEdit: Edit;
        TNavigateBtn.nbCancel: Cancel;
        TNavigateBtn.nbPost: Post;
        TNavigateBtn.nbRefresh: Refresh;
        TNavigateBtn.nbDelete:
          if not FConfirmDelete or
            (StyledMessageDlg(SDeleteRecordQuestion, mtConfirmation,
            mbOKCancel, 0) <> idCancel) then Delete;
        TNavigateBtn.nbApplyUpdates: Self.ApplyUpdates;
        TNavigateBtn.nbCancelUpdates: Self.CancelUpdates;
      end;
    end;
  end;
  if not (csDesigning in ComponentState) and Assigned(FOnNavClick) then
    FOnNavClick(Self, Index);
end;

function TStyledDBNavigator.CanApplyUpdates: Boolean;
var
  Intf: IDataSetCommandSupport;
begin
  if (Self.DataSource <> nil) and Supports(Self.DataSource.DataSet, IDataSetCommandSupport, Intf) then
    Result := dcEnabled in Intf.GetCommandStates(sApplyUpdatesDataSetCommand)
  else
    Result := False;
end;

function TStyledDBNavigator.CanCancelUpdates: Boolean;
var
  Intf: IDataSetCommandSupport;
begin
  if (Self.DataSource <> nil) and Supports(Self.DataSource.DataSet, IDataSetCommandSupport, Intf) then
    Result := dcEnabled in Intf.GetCommandStates(sCancelUpdatesDataSetCommand)
  else
    Result := False;
end;

procedure TStyledDBNavigator.CancelUpdates;
var
  Intf: IDataSetCommandSupport;
begin
  if (Self.DataSource <> nil) and Supports(Self.DataSource.DataSet, IDataSetCommandSupport, Intf) then
    Intf.ExecuteCommand(sCancelUpdatesDataSetCommand, [MaxErrors])
end;

procedure TStyledDBNavigator.ClickHandler(Sender: TObject);
begin
  inherited;
  BtnClick(TStyledNavButton(Sender).Index);
end;

procedure TStyledDBNavigator.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then
    ActiveChanged;
end;

constructor TStyledDBNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TStyledNavDataLink.Create(Self);
end;

procedure TStyledDBNavigator.DataChanged;
var
  UpEnable, DnEnable: Boolean;
  CanModify, CanRefresh: Boolean;
begin
  CanModify := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;
  CanRefresh := Enabled and FDataLink.Active and FDataLink.DataSet.CanRefresh;
  UpEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.BOF;
  DnEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.EOF;
  FButtons[TNavigateBtn.nbFirst].EnabledNavBtn(UpEnable);
  FButtons[TNavigateBtn.nbPrior].EnabledNavBtn(UpEnable);
  FButtons[TNavigateBtn.nbNext].EnabledNavBtn(DnEnable);
  FButtons[TNavigateBtn.nbLast].EnabledNavBtn(DnEnable);
  FButtons[TNavigateBtn.nbInsert].EnabledNavBtn(CanModify);
  FButtons[TNavigateBtn.nbEdit].EnabledNavBtn(CanModify and
    not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF));
  FButtons[TNavigateBtn.nbDelete].EnabledNavBtn(CanModify and
    not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF));
  FButtons[TNavigateBtn.nbRefresh].EnabledNavBtn(CanRefresh);
  FButtons[TNavigateBtn.nbApplyUpdates].EnabledNavBtn(CanModify and Self.CanApplyUpdates);
  FButtons[TNavigateBtn.nbCancelUpdates].EnabledNavBtn(CanModify and Self.CanCancelUpdates);
end;

destructor TStyledDBNavigator.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

procedure TStyledDBNavigator.EditingChanged;
var
  CanModify: Boolean;
begin
  CanModify := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;
  Buttons[TNavigateBtn.nbInsert].EnabledNavBtn(CanModify);
  Buttons[TNavigateBtn.nbEdit].EnabledNavBtn(CanModify and not FDataLink.Editing);
  Buttons[TNavigateBtn.nbPost].EnabledNavBtn(CanModify and FDataLink.Editing);
  Buttons[TNavigateBtn.nbCancel].EnabledNavBtn(CanModify and FDataLink.Editing);
  Buttons[TNavigateBtn.nbRefresh].EnabledNavBtn(Enabled and (TNavigateBtn.nbRefresh in VisibleButtons) and FDataLink.Active and FDataLink.DataSet.CanRefresh);
  Buttons[TNavigateBtn.nbApplyUpdates].EnabledNavBtn(CanModify and (TNavigateBtn.nbApplyUpdates in VisibleButtons) and Self.CanApplyUpdates);
  Buttons[TNavigateBtn.nbCancelUpdates].EnabledNavBtn(CanModify and (TNavigateBtn.nbCancelUpdates in VisibleButtons) and Self.CanCancelUpdates);
end;

function TStyledDBNavigator.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TStyledDBNavigator.Loaded;
begin
  inherited;
  ActiveChanged;
end;

procedure TStyledDBNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil;
  end;
end;

procedure TStyledDBNavigator.SetDataSource(const AValue: TDataSource);
begin
  FDataLink.DataSource := AValue;
  if not (csLoading in ComponentState) then
    ActiveChanged;
  if AValue <> nil then
    AValue.FreeNotification(Self);
end;

{ TStyledNavButton }

constructor TStyledNavButton.Create(AOwner: TComponent);
begin
  if AOwner is TCustomStyledDbNavigator then
  begin
    FDbNavigator := TCustomStyledDbNavigator(AOwner);
    inherited CreateStyled(AOwner,
      FDbNavigator._DefaultFamily, FDbNavigator._DefaultClass,
      FDbNavigator._DefaultAppearance,
      FDbNavigator._DefaultStyleDrawType,
      FDbNavigator._DefaultCursor,
      FDbNavigator._UseCustomDrawType);
    StyleRadius := FDbNavigator.StyleRadius;
    StyleDrawType := FDbNavigator.StyleDrawType;
    ControlStyle := [csCaptureMouse, csDoubleClicks, csSetCaption, csOpaque];
  end
  else
    inherited Create(AOwner);
  //ControlStyle := ControlStyle + [csCaptureMouse];
  ImageAlignment := iaTop;
end;

destructor TStyledNavButton.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  inherited Destroy;
end;

function TStyledNavButton.GetCaptionToDraw: TCaption;
begin
  if Assigned(FDbNavigator) and not FDbNavigator.ShowCaptions then
    Result := ''
  else
    Result := inherited GetCaption;
end;

function TStyledNavButton.IsCustomDrawType: Boolean;
begin
  if Assigned(FDbNavigator) then
    Result := StyleDrawType <> FDbNavigator.StyleDrawType
  else
    Result := StyleDrawType <> btRoundRect;
end;

function TStyledNavButton.IsCustomRadius: Boolean;
begin
  if Assigned(FDbNavigator) then
    Result := StyleRadius <> FDbNavigator.StyleRadius
  else
    Result := StyleRadius <> DEFAULT_RADIUS;
end;

function TStyledNavButton.IsCustomRoundedCorners: Boolean;
begin
  Result := StyleRoundedCorners <> ALL_ROUNDED_CORNERS;
end;

function TStyledNavButton.IsImageAlignmentStored: Boolean;
begin
  if Assigned(FDbNavigator) then
    Result := FImageAlignment <> iaTop
  else
    Result := True;
end;

function TStyledNavButton.IsStoredStyleFamily: Boolean;
begin
  if Assigned(FDbNavigator) then
    Result := not SameText(StyleFamily,FDbNavigator.StyleFamily)
  else
    Result := True;
end;

function TStyledNavButton.IsStoredStyleAppearance: Boolean;
begin
  if Assigned(FDbNavigator) then
    Result := not SameText(StyleAppearance, FDbNavigator.StyleAppearance)
  else
    Result := True;
end;

function TStyledNavButton.IsStoredStyleClass: Boolean;
begin
  if Assigned(FDbNavigator) then
    Result := not SameText(StyleClass,FDbNavigator.StyleClass)
  else
    Result := inherited IsStoredStyleClass;
end;

procedure TStyledNavButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if nsAllowTimer in FNavStyle then
  begin
    if FRepeatTimer = nil then
      FRepeatTimer := TTimer.Create(Self);

    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled := True;
  end;
  if (Button = mbLeft) and Enabled then
  begin
    FDragging := True;
  end;
end;

procedure TStyledNavButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DoClick: Boolean;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled := False;
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    if DoClick then Click;
  end;
end;

procedure TStyledNavButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (Buttonstate = bsmPressed) and (MouseInControl) then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

procedure TStyledNavButton.UpdateButtonContent;
begin
  //Updates content of Button based on ShowCaptions and Style
  if Assigned(FDbNavigator) then
  begin
    if FDbNavigator.ShowCaptions then
    begin
      inherited ImageAlignment := FImageAlignment;
      case FImageAlignment of
        iaLeft: inherited ImageMargins.Left := DEFAULT_IMAGE_MARGIN;
        iaRight: inherited ImageMargins.Right := DEFAULT_IMAGE_MARGIN;
        iaTop: inherited ImageMargins.Top := DEFAULT_IMAGE_MARGIN;
        iaBottom: inherited ImageMargins.Bottom := DEFAULT_IMAGE_MARGIN;
      end;
    end
    else
    begin
      inherited ImageAlignment := iaCenter;
    end;
    if Assigned(FDbNavigator.Images) then
      inherited Images := FDbNavigator.Images
    else
      inherited Images := {$IFDEF D10_4+}FDbNavigator.FButtonImages;{$ELSE}nil;{$ENDIF}
    inherited DisabledImages := FDbNavigator.DisabledImages;
    StyleElements := FDbNavigator.StyleElements;
  end;
  Invalidate;
end;

procedure TStyledNavButton.Paint;
begin
  inherited Paint;
end;

procedure TStyledNavButton.SetCaption(const AValue: TCaption);
begin
  if AValue <> (inherited Caption) then
  begin
    inherited SetCaption(AValue);
    Invalidate;
  end;
end;

procedure TStyledNavButton.EnabledNavBtn(AValue: Boolean);
begin
  if Assigned(FDbNavigator) and Assigned(FDbNavigator.FOnEnableNavBtn) then
    FDbNavigator.FOnEnableNavBtn(FDbNavigator, Self, AValue);
  if AValue <> Enabled then
    Enabled := AValue;
end;

procedure TStyledNavButton.SetImageAlignment(const AValue: TImageAlignment);
begin
  if FImageAlignment <> AValue then
  begin
    FImageAlignment := AValue;
    UpdateButtonContent;
  end;
end;

{ TStyledNavDataLink }

constructor TStyledNavDataLink.Create(ANav: TStyledDBNavigator);
begin
  inherited Create;
  FNavigator := ANav;
  VisualControl := True;
end;

destructor TStyledNavDataLink.Destroy;
begin
  FNavigator := nil;
  inherited Destroy;
end;

procedure TStyledNavDataLink.EditingChanged;
begin
  if FNavigator <> nil then
    FNavigator.EditingChanged;
end;

procedure TStyledNavDataLink.DataSetChanged;
begin
  if FNavigator <> nil then
    FNavigator.DataChanged;
end;

procedure TStyledNavDataLink.ActiveChanged;
begin
  if FNavigator <> nil then
    FNavigator.ActiveChanged;
end;

var
  NavigateButtonHintId: array[TNavigateButton] of string = (SFirstRecord, SPriorRecord,
    SNextRecord, SLastRecord, SInsertRecord, SDeleteRecord, SEditRecord,
    SPostEdit, SCancelEdit, SRefreshRecord,
    SApplyUpdates, SCancelUpdates);

{ TStyledBindNavigator }

procedure TStyledBindNavigator.ClickHandler(Sender: TObject);
begin
  inherited;
  BtnClick(NavBtnToNavigateButton(TStyledNavButton(Sender).Index));
end;

constructor TStyledBindNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  VisibleButtons := NavigatorDefaultButtons;
  FController := TBindNavigatorController.Create(Self);
  FController.OnEditingChanged := OnEditingChanged;
  FController.OnDataChanged := OnDataChanged;
  FController.OnActiveChanged := OnActiveChanged;
end;

procedure TStyledBindNavigator.ActiveChanged;
var
  LActive: Boolean;
begin
  LActive := FController.Active;
  if not (Enabled and LActive) then
    FController.DisableButtons(
      procedure(AButton: TNavigateButton; AEnabled: Boolean)
      begin
        Buttons[AButton].EnabledNavBtn(AEnabled);
      end)
  else
  begin
    FController.EnableButtons(NavigatorButtons, Self.Enabled,
      procedure(AButton: TNavigateButton; AEnabled: Boolean)
      begin
        Buttons[AButton].EnabledNavBtn(AEnabled);
      end)
  end;
end;

procedure TStyledBindNavigator.DataChanged;
begin
  FController.EnableButtons(NavigatorScrollButtons +
    [nbFirst, nbPrior, nbNext, nbLast,
     nbInsert, nbDelete, nbEdit, nbPost, nbCancel, nbRefresh, nbApplyUpdates,
     nbCancelUpdates], Self.Enabled,
    procedure(AButton: TNavigateButton; AEnabled: Boolean)
    begin
      Buttons[AButton].EnabledNavBtn(AEnabled);
    end);
end;

destructor TStyledBindNavigator.Destroy;
begin
  FreeAndNil(FController);
  inherited;
end;

procedure TStyledBindNavigator.EditingChanged;
begin
  FController.EnableButtons(NavigatorEditButtons - [nbDelete], Enabled,
    procedure(AButton: TNavigateButton; AEnabled: Boolean)
    begin
      Buttons[AButton].EnabledNavBtn(AEnabled);
    end);
end;

procedure TStyledBindNavigator.OnEditingChanged(Sender: TObject);
begin
  EditingChanged;
end;

procedure TStyledBindNavigator.OnActiveChanged(Sender: TObject);
begin
  ActiveChanged;
end;

procedure TStyledBindNavigator.OnDataChanged(Sender: TObject);
begin
  DataChanged;
end;

procedure TStyledBindNavigator.SetDataSource(Value: TBaseLinkingBindSource);
begin
  if FController.DataSource <> Value then
  begin
    FController.DataSource := Value;
    if not (csLoading in ComponentState) then
      ActiveChanged;
  end;
end;

function TStyledBindNavigator.GetOrientation: TNavigatorOrientation;
begin
  if inherited Kind = dbnHorizontal then
    Result := orHorizontal
  else
    Result := orVertical;
end;

function TStyledBindNavigator.GetVisibleButtons: TNavigateButtons;
begin
  Result := NavBtnsToNavigateButtons(inherited VisibleButtons);
end;

procedure TStyledBindNavigator.SetOrientation(const Value: TNavigatorOrientation);
begin
  if Value = orHorizontal then
    inherited Kind := dbnHorizontal
  else
    inherited Kind := dbnVertical;
end;

function TStyledBindNavigator.NavigateButtonsToNavBtns(
  const AValue: TNavigateButtons): TNavButtonSet;
begin
  Result := [];
  if TNavigateButton.nbFirst in AValue then Result := Result + [TNavigateBtn.nbFirst];
  if TNavigateButton.nbPrior in AValue then Result := Result + [TNavigateBtn.nbPrior];
  if TNavigateButton.nbNext in AValue then Result := Result + [TNavigateBtn.nbNext];
  if TNavigateButton.nbLast in AValue then Result := Result + [TNavigateBtn.nbLast];
  if TNavigateButton.nbInsert in AValue then Result := Result + [TNavigateBtn.nbInsert];
  if TNavigateButton.nbDelete in AValue then Result := Result + [TNavigateBtn.nbDelete];
  if TNavigateButton.nbEdit in AValue then Result := Result + [TNavigateBtn.nbEdit];
  if TNavigateButton.nbPost in AValue then Result := Result + [TNavigateBtn.nbPost];
  if TNavigateButton.nbCancel in AValue then Result := Result + [TNavigateBtn.nbCancel];
  if TNavigateButton.nbRefresh in AValue then Result := Result + [TNavigateBtn.nbRefresh];
  if TNavigateButton.nbApplyUpdates in AValue then Result := Result + [TNavigateBtn.nbApplyUpdates];
  if TNavigateButton.nbCancelUpdates in AValue then Result := Result + [TNavigateBtn.nbCancelUpdates];
end;

function TStyledBindNavigator.NavBtnsToNavigateButtons(
  const AValue: TNavButtonSet): TNavigateButtons;
begin
  Result := [];
  if TNavigateBtn.nbFirst in AValue then Result := Result + [TNavigateButton.nbFirst];
  if TNavigateBtn.nbPrior in AValue then Result := Result + [TNavigateButton.nbPrior];
  if TNavigateBtn.nbNext in AValue then Result := Result + [TNavigateButton.nbNext];
  if TNavigateBtn.nbLast in AValue then Result := Result + [TNavigateButton.nbLast];
  if TNavigateBtn.nbInsert in AValue then Result := Result + [TNavigateButton.nbInsert];
  if TNavigateBtn.nbDelete in AValue then Result := Result + [TNavigateButton.nbDelete];
  if TNavigateBtn.nbEdit in AValue then Result := Result + [TNavigateButton.nbEdit];
  if TNavigateBtn.nbPost in AValue then Result := Result + [TNavigateButton.nbPost];
  if TNavigateBtn.nbCancel in AValue then Result := Result + [TNavigateButton.nbCancel];
  if TNavigateBtn.nbRefresh in AValue then Result := Result + [TNavigateButton.nbRefresh];
  if TNavigateBtn.nbApplyUpdates in AValue then Result := Result + [TNavigateButton.nbApplyUpdates];
  if TNavigateBtn.nbCancelUpdates in AValue then Result := Result + [TNavigateButton.nbCancelUpdates];
end;

function TStyledBindNavigator.NavigateButtonToNavBtn(
  const AValue: TNavigateButton): TNavigateBtn;
begin
  case AValue of
    TNavigateButton.nbFirst:         Result := TNavigateBtn.nbFirst;
    TNavigateButton.nbPrior:         Result := TNavigateBtn.nbPrior;
    TNavigateButton.nbNext:          Result := TNavigateBtn.nbNext;
    TNavigateButton.nbLast:          Result := TNavigateBtn.nbLast;
    TNavigateButton.nbInsert:        Result := TNavigateBtn.nbInsert;
    TNavigateButton.nbDelete:        Result := TNavigateBtn.nbDelete;
    TNavigateButton.nbEdit:          Result := TNavigateBtn.nbEdit;
    TNavigateButton.nbPost:          Result := TNavigateBtn.nbPost;
    TNavigateButton.nbCancel:        Result := TNavigateBtn.nbCancel;
    TNavigateButton.nbRefresh:       Result := TNavigateBtn.nbRefresh;
    TNavigateButton.nbApplyUpdates:  Result := TNavigateBtn.nbApplyUpdates;
    TNavigateButton.nbCancelUpdates: Result := TNavigateBtn.nbCancelUpdates;
  else
    Result :=  TNavigateBtn.nbFirst;
  end;
end;

function TStyledBindNavigator.NavBtnToNavigateButton(
  const AValue: TNavigateBtn): TNavigateButton;
begin
  case AValue of
    TNavigateBtn.nbFirst        : Result := TNavigateButton.nbFirst;
    TNavigateBtn.nbPrior        : Result := TNavigateButton.nbPrior;
    TNavigateBtn.nbNext         : Result := TNavigateButton.nbNext;
    TNavigateBtn.nbLast         : Result := TNavigateButton.nbLast;
    TNavigateBtn.nbInsert       : Result := TNavigateButton.nbInsert;
    TNavigateBtn.nbDelete       : Result := TNavigateButton.nbDelete;
    TNavigateBtn.nbEdit         : Result := TNavigateButton.nbEdit;
    TNavigateBtn.nbPost         : Result := TNavigateButton.nbPost;
    TNavigateBtn.nbCancel       : Result := TNavigateButton.nbCancel;
    TNavigateBtn.nbRefresh      : Result := TNavigateButton.nbRefresh;
    TNavigateBtn.nbApplyUpdates : Result := TNavigateButton.nbApplyUpdates;
    TNavigateBtn.nbCancelUpdates: Result := TNavigateButton.nbCancelUpdates;
  else
    Result :=  TNavigateButton.nbFirst;
  end;
end;

procedure TStyledBindNavigator.SetVisible(const Value: TNavigateButtons);
begin
  inherited VisibleButtons := NavigateButtonsToNavBtns(Value);
end;

function TStyledBindNavigator.GetDataSource: TBaseLinkingBindSource;
begin
  Result := FController.DataSource as TBaseLinkingBindSource;
end;

function TStyledBindNavigator.GetButton(Index: TNavigateButton): TStyledNavButton;
begin
  Result := inherited GetButton(NavigateButtonToNavBtn(Index));
end;

procedure TStyledBindNavigator.BtnClick(Index: TNavigateButton);
begin
  if (DataSource <> nil) then
  begin
    if not (csDesigning in ComponentState) and Assigned(BeforeAction) then
      BeforeAction(Self, Index);
    FController.ExecuteButton(Index,
       function: Boolean
       begin
          Result := not ConfirmDelete or
          (MessageDlg(SDeleteRecordQuestion, mtConfirmation,
          mbOKCancel, 0) <> idCancel);
       end
      );
  end;
  if not (csDesigning in ComponentState) and Assigned(OnClick) then
    OnClick(Self, Index);
end;

initialization
  TCustomStyledDBNavigator._DefaultStyleDrawType := DEFAULT_STYLEDRAWTYPE;
  TCustomStyledDBNavigator._DefaultFamily := DEFAULT_CLASSIC_FAMILY;
  TCustomStyledDBNavigator._DefaultClass := DEFAULT_WINDOWS_CLASS;
  TCustomStyledDBNavigator._DefaultAppearance := DEFAULT_APPEARANCE;
  TCustomStyledDBNavigator._DefaultStyleRadius := DEFAULT_RADIUS;
  TCustomStyledDBNavigator._DefaultCursor := DEFAULT_CURSOR;

end.
