{******************************************************************************}
{                                                                              }
{       StyledDbNavigator: a DbNavigator with TStyledNavButtons inside         }
{       Based on TStyledToolbar                                                }
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
  , Vcl.DBCtrls
  , Data.db
{$IFDEF D10_4+}
  , Vcl.VirtualImageList
  , Vcl.ImageCollection
{$ENDIF}
  ;

resourcestring
  ERROR_SETTING_DBNAVIGATOR_STYLE = 'Error setting DbNavigator Style: %s/%s/%s not available';

type
  EStyledDbnavigatorError = Exception;

  TStyledDbNavigator = class;
  TStyledNavButton = class;
  TStyledNavDataLink = class;

  TButtonProc = reference to procedure (Button: TStyledNavButton);
  TNavButtons = array[TNavigateBtn] of TStyledNavButton;

  { TStyledNavButton }

  TStyledNavButton = class(TStyledGraphicButton)
  private
    FIndex: TNavigateBtn;
    FNavStyle: TNavButtonStyle;
    FRepeatTimer: TTimer;
    FDbNavigator: TStyledDBNavigator;
    FImageAlignment: TImageAlignment;
    procedure TimerExpired(Sender: TObject);
    procedure UpdateButtonContent;
    function IsImageAlignmentStored: Boolean;
    procedure SetImageAlignment(const AValue: TImageAlignment);
  protected
    function GetCaption: TCaption; override;
    procedure SetCaption(const AValue: TCaption); override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property NavStyle: TNavButtonStyle read FNavStyle write FNavStyle;
    property Index : TNavigateBtn read FIndex write FIndex;
  published
    property ImageAlignment: TImageAlignment read FImageAlignment write SetImageAlignment Stored IsImageAlignmentStored;
    property Caption: TCaption read GetCaption write SetCaption stored IsCaptionStored;
  end;

  { TStyledDBNavigator }

  TStyledDBNavigator = class (TCustomPanel)
  private
    //Standard support ad TDbNavigator
    FDataLink: TStyledNavDataLink;
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

    //Imagelist support
    FImages: TCustomImageList;
    FDisabledImages: TCustomImageList;
    FShowCaptions: Boolean;

    {$IFDEF D10_4+}
    FButtonImages: TVirtualImageList;
    //Internal ImageList and Collection for standard images
    class var FButtonsImageCollection: TImageCollection;
    class constructor Create;
    class destructor Destroy;
    procedure UpdateButtonsImageIndex;
    {$ELSE}
    procedure UpdateButtonsGlyphs;
    {$ENDIF}

    procedure ImageListChange(Sender: TObject);
    procedure DisabledImageListChange(Sender: TObject);
    procedure ProcessButtons(AButtonProc: TButtonProc);

    function GetActiveStyleName: string;
    function AsVCLStyle: Boolean;
    function ApplyDbnavigatorStyle: Boolean;

    procedure BtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ClickHandler(Sender: TObject);
    function GetDataSource: TDataSource;
    function GetHints: TStrings;
    procedure HintsChanged(Sender: TObject);
    procedure CaptionsChanged(Sender: TObject);
    procedure InitButtons;
    procedure InitHints;
    procedure InitCaptions;
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetFlat(const AValue: Boolean);
    procedure SetHints(const AValue: TStrings);
    procedure SetKind(const AValue: TDBNavigatorKind);
    procedure SetSize(var W: Integer; var H: Integer);
    procedure SetVisible(const AValue: TNavButtonSet);
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure ApplyUpdates;
    function CanApplyUpdates: Boolean;
    procedure CancelUpdates;
    function CanCancelUpdates: Boolean;
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
  protected
    procedure UpdateStyleElements; override;
    procedure ActiveChanged;
    procedure CalcMinSize(var W, H: Integer);
    procedure CreateWnd; override;
    procedure DataChanged;
    procedure EditingChanged;
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
    procedure BtnClick(Index: TNavigateBtn); virtual;
    property StyleApplied: Boolean read FStyleApplied write SetStyleApplied;
    //Access readonly properties
    property Buttons: TNavButtons read FButtons;
    property ButtonWidth: Integer read FButtonWidth;
    property ButtonHeight: Integer  read FButtonHeight;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property VisibleButtons: TNavButtonSet read FVisibleButtons write SetVisible
      default [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete,
        nbEdit, nbPost, nbCancel, nbRefresh];
    property MaxErrors: Integer read FMaxErrors write FMaxErrors default -1;
    property Align;
    property Anchors;
    property Constraints;
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

    property ShowCaptions: Boolean read FShowCaptions write SetShowCaptions default False;
    //StyledComponents Attributes
    property StyleRadius: Integer read FStyleRadius write SetStyleRadius stored IsCustomRadius;
    property StyleDrawType: TStyledButtonDrawType read FStyleDrawType write SetStyleDrawType stored IsCustomDrawType;
    property StyleFamily: TStyledButtonFamily read FStyleFamily write SetStyleFamily stored IsStoredStyleFamily;
    property StyleClass: TStyledButtonClass read FStyleClass write SetStyleClass stored IsStoredStyleClass;
    property StyleAppearance: TStyledButtonAppearance read FStyleAppearance write SetStyleAppearance stored IsStoredStyleAppearance;
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


implementation

uses
  Vcl.Consts
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.Graphics
  , System.Types
  , System.RTLConsts
  , Vcl.StandardButtonStyles
  , Vcl.StyledCmpMessages
  , Vcl.StyledTaskDialog
  ;

const
  DEFAULT_BTN_IMAGE_SIZE = 15;

{ TStyledDBNavigator }

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
class constructor TStyledDBNavigator.Create;
begin
end;

procedure InitButtonsImageCollection;
var
  I: TNavigateBtn;
  LBtnName, LResName: string;
begin
  if TStyledDBNavigator.FButtonsImageCollection <> nil then
    Exit;
  TStyledDBNavigator.FButtonsImageCollection := TImageCollection.Create(nil);
  for I := Low(BtnTypeName) to High(BtnTypeName) do
  begin
    LBtnName := BtnTypeName[I];
    LResName := 'SNB_' + BtnTypeName[I];
    TStyledDBNavigator.FButtonsImageCollection.Add(LBtnName, HInstance,
      LResName, ['', '_20X']);
  end;
  //Add also vertical images
  for I := nbFirst to nbLast do
  begin
    LBtnName := BtnTypeName[I] + '_VERT';
    LResName := 'SNB_' + BtnTypeName[I] + '_VERT';
    TStyledDBNavigator.FButtonsImageCollection.Add(LBtnName, HInstance,
      LResName, ['', '_20X']);
  end;
end;
{$ENDIF}

constructor TStyledDBNavigator.Create(AOwner: TComponent);
begin
  CreateStyled(AOwner,
    DEFAULT_CLASSIC_FAMILY,
    DEFAULT_WINDOWS_CLASS,
    DEFAULT_APPEARANCE);
end;

constructor TStyledDBNavigator.CreateStyled(AOwner: TComponent;
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
  FDataLink := TStyledNavDataLink.Create(Self);
  FVisibleButtons := [nbFirst, nbPrior, nbNext, nbLast, nbInsert,
    nbDelete, nbEdit, nbPost, nbCancel, nbRefresh];
  FHints := TStringList.Create;
  FCaptions := TStringList.Create;
  TStringList(FHints).OnChange := HintsChanged;
  TStringList(FCaptions).OnChange := CaptionsChanged;
  {$IFDEF D10_4+}
  FButtonImages := TVirtualImageList.Create(Self);
  FButtonImages.SetSize(DEFAULT_BTN_IMAGE_SIZE, DEFAULT_BTN_IMAGE_SIZE);
  FButtonImages.AutoFill := True;
  FButtonImages.ImageCollection := FButtonsImageCollection;
  {$ENDIF}

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
  FocusedButton := nbFirst;
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

  FStyleDrawType := btRounded;
  FStyleRadius := DEFAULT_RADIUS;
  FStyleFamily := AFamily;
  FStyleClass := AClass;
  FStyleAppearance := AAppearance;
end;

{$IFDEF D10_4+}


{$ELSE}
procedure TStyledDBNavigator.UpdateButtonsGlyphs;
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

procedure TStyledDBNavigator.CreateWnd;
begin
  inherited;
  {$IFNDEF D10_4+}
  UpdateButtonsGlyphs;
  {$ENDIF}
end;

destructor TStyledDBNavigator.Destroy;
begin
  FreeAndNil(FDefaultHints);
  FreeAndNil(FDefaultCaptions);
  FreeAndNil(FDataLink);
  FreeAndNil(FHints);
  FreeAndNil(FCaptions);
  FreeAndNil(FDisabledImageChangeLink);
  FreeAndNil(FImageChangeLink);
  inherited Destroy;
end;

{$IFDEF D10_4+}
class destructor TStyledDBNavigator.Destroy;
begin
  FreeAndNil(FButtonsImageCollection);
end;

procedure TStyledDBNavigator.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  FButtonImages.SetSize(MulDiv(FButtonImages.Width, M, D), MulDiv(FButtonImages.Height, M, D));
end;

procedure TStyledDBNavigator.UpdateButtonsImageIndex;
var
  I: TNavigateBtn;
  Btn: TStyledNavButton;
begin
  for I := Low(FButtons) to High(FButtons) do
  begin
    Btn := Buttons[I];
    if (FKind = dbnVertical) and (I in [nbFirst, nbPrior, nbNext, nbLast]) then
      Btn.ImageIndex := Ord(I) + Ord(High(FButtons)) +1
    else
      Btn.ImageIndex := Ord(I);
  end;
end;
{$ELSE}
procedure TStyledDBNavigator.SetButtonGlyph(Index: TNavigateBtn);
var
  LResName: string;
begin
  FmtStr(LResName, 'SNB_%s', [BtnTypeName[Index]]);
  FButtons[Index].NumGlyphs := 2;
  FButtons[Index].Glyph.LoadFromResourceName(HInstance, LResName);
end;
{$ENDIF}

procedure TStyledDBNavigator.Paint;
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

procedure TStyledDBNavigator.ImageListChange(Sender: TObject);
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
  UpdateButtonsImageIndex;
  {$ELSE}
  UpdateButtonsGlyphs;
  {$ENDIF}
end;

procedure TStyledDBNavigator.DisabledImageListChange(Sender: TObject);
begin
  ProcessButtons(
    procedure (ABtn: TStyledNavButton)
    begin
      ABtn.DisabledImages := DisabledImages;
    end
  );
end;

procedure TStyledDBNavigator.ProcessButtons(AButtonProc: TButtonProc);
var
  I: TNavigateBtn;
begin
  for I := Low(FButtons) to High(FButtons) do
    AButtonProc(FButtons[I]);
end;

procedure TStyledDBNavigator.InitButtons;
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
    Btn.Enabled := True;
    Btn.SetBounds (X, Y, FMinBtnSize.X, FMinBtnSize.Y);
    Btn.OnClick := ClickHandler;
    Btn.OnMouseDown := BtnMouseDown;
    Btn.Parent := Self;
    FButtons[I] := Btn;
    if Kind = dbnHorizontal then
      X := X + FMinBtnSize.X
    else
      Y := Y + FMinBtnSize.Y;
  end;
  FButtons[nbPrior].NavStyle := FButtons[nbPrior].NavStyle + [nsAllowTimer];
  FButtons[nbNext].NavStyle  := FButtons[nbNext].NavStyle + [nsAllowTimer];
  {$IFDEF D10_4+}
  UpdateButtonsImageIndex;
  {$ELSE}
  UpdateButtonsGlyphs;
  {$ENDIF}
end;

procedure TStyledDBNavigator.InitCaptions;
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

procedure TStyledDBNavigator.InitHints;
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

function TStyledDBNavigator.IsCustomDrawType: Boolean;
begin
  Result := FCustomDrawType;
end;

function TStyledDBNavigator.IsCustomRadius: Boolean;
begin
  Result := StyleRadius <> DEFAULT_RADIUS;
end;

function TStyledDBNavigator.IsStoredStyleAppearance: Boolean;
var
  LClass: TStyledButtonClass;
  LAppearance: TStyledButtonAppearance;
  LButtonFamily: TButtonFamily;
begin
  StyleFamilyCheckAttributes(FStyleFamily, LClass, LAppearance, LButtonFamily);
  Result := FStyleAppearance <> LAppearance;
end;

function TStyledDBNavigator.IsStoredStyleClass: Boolean;
var
  LClass: TStyledButtonClass;
  LAppearance: TStyledButtonAppearance;
  LButtonFamily: TButtonFamily;
begin
  StyleFamilyCheckAttributes(FStyleFamily, LClass, LAppearance, LButtonFamily);

  if (FStyleFamily = DEFAULT_CLASSIC_FAMILY) and (seClient in StyleElements) then
  begin
    Result := (FStyleClass <> GetActiveStyleName)
      and not SameText(FStyleClass, 'Windows');
  end
  else
    Result := FStyleClass <> LClass;
end;

function TStyledDBNavigator.IsStoredStyleFamily: Boolean;
begin
  Result := FStyleFamily <> DEFAULT_CLASSIC_FAMILY;
end;

procedure TStyledDBNavigator.HintsChanged(Sender: TObject);
begin
  InitHints;
end;

procedure TStyledDBNavigator.CaptionsChanged(Sender: TObject);
begin
  InitCaptions;
end;

procedure TStyledDBNavigator.SetFlat(const AValue: Boolean);
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

procedure TStyledDBNavigator.SetHints(const AValue: TStrings);
begin
  if AValue.Text = FDefaultHints.Text then
    FHints.Clear else
    FHints.Assign(AValue);
end;

procedure TStyledDBNavigator.SetCaptions(const AValue: TStrings);
begin
  if AValue.Text = FDefaultCaptions.Text then
    FCaptions.Clear else
    FCaptions.Assign(AValue);
end;

procedure TStyledDBNavigator.SetImages(const AValue: TCustomImageList);
begin
  if FImages <> AValue then
  begin
    FImages := AValue;
    ImageListChange(Self);
  end;
end;

function TStyledDBNavigator.GetHints: TStrings;
begin
  if (csDesigning in ComponentState) and not (csWriting in ComponentState) and
     not (csReading in ComponentState) and (FHints.Count = 0) then
    Result := FDefaultHints else
    Result := FHints;
end;

function TStyledDBNavigator.GetCaptions: TStrings;
begin
  if (csDesigning in ComponentState) and not (csWriting in ComponentState) and
     not (csReading in ComponentState) and (FCaptions.Count = 0) then
    Result := FDefaultCaptions else
    Result := FCaptions;
end;

procedure TStyledDBNavigator.SetKind(const AValue: TDBNavigatorKind);
begin
  if FKind <> AValue then
  begin
    FKind := AValue;
    if not (csLoading in ComponentState) then
      SetBounds(Left, Top, Height, Width);
    {$IFDEF D10_4+}
    UpdateButtonsImageIndex;
    {$ELSE}
    UpdateButtonsGlyphs;
    {$ENDIF}
    Invalidate;
  end;
end;

function TStyledDBNavigator.GetActiveStyleName: string;
begin
  {$IFDEF D10_4+}
  Result := GetStyleName;
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

procedure TStyledDBNavigator.GetChildren(Proc: TGetChildProc; Root: TComponent);
//var
//  J: TNavigateBtn;
begin
//  for J := Low(FButtons) to High(FButtons) do
//    Proc(FButtons[J]);
end;

procedure TStyledDBNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil;
    if (AComponent = Images) then
      Images := {$IFDEF D10_4+}FButtonImages;{$ELSE}nil;{$ENDIF}
    if (AComponent = DisabledImages) then
      DisabledImages := nil;
  end;
end;

procedure TStyledDBNavigator.SetVisible(const AValue: TNavButtonSet);
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

procedure TStyledDBNavigator.UpdateStyleElements;
var
  LStyleClass: TStyledButtonClass;
begin
  if AsVCLStyle then
  begin
    //if StyleElements contains seClient then Update style
    //as VCL Style assigned to Toolbar or Global VCL Style
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

procedure TStyledDBNavigator.CalcMinSize(var W, H: Integer);
var
  Count: Integer;
  I: TNavigateBtn;
begin
  if (csLoading in ComponentState) then Exit;
  if FButtons[nbFirst] = nil then Exit;

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

procedure TStyledDBNavigator.UpdateButtons;
begin
  ProcessButtons(
    procedure (ABtn: TStyledNavButton)
    begin
      ABtn.UpdateButtonContent;
    end);
end;

procedure TStyledDBNavigator.SetShowCaptions(const AValue: Boolean);
begin
  if FShowCaptions <> AValue then
  begin
    FShowCaptions := AValue;
    UpdateButtons;
  end;
end;

procedure TStyledDBNavigator.SetSize(var W: Integer; var H: Integer);
var
  Count: Integer;
  I: TNavigateBtn;
  Space, Temp, Remain: Integer;
  X, Y: Integer;
begin
  if (csLoading in ComponentState) then
    Exit;
  if FButtons[nbFirst] = nil then
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

procedure TStyledDBNavigator.SetStyleAppearance(
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

procedure TStyledDBNavigator.SetStyleApplied(const AValue: Boolean);
begin
  FStyleApplied := AValue;
end;

procedure TStyledDBNavigator.SetStyleClass(const AValue: TStyledButtonClass);
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
        if ABtn.StyleClass = StyleClass then
          ABtn.StyleClass := LValue;
      end);
    FStyleClass := LValue;
    StyleApplied := ApplyDbnavigatorStyle;
  end;
end;

procedure TStyledDBNavigator.SetStyleDrawType(
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

procedure TStyledDBNavigator.SetStyleFamily(const AValue: TStyledButtonFamily);
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
  end;
  if FStyleFamily = DEFAULT_CLASSIC_FAMILY then
    StyleElements := [seFont, seClient, seBorder];
end;

procedure TStyledDBNavigator.SetStyleRadius(const AValue: Integer);
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

procedure TStyledDBNavigator.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  if not HandleAllocated then SetSize(W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end;

procedure TStyledDBNavigator.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;
  W := Width;
  H := Height;
  SetSize(W, H);
end;

procedure TStyledDBNavigator.WMWindowPosChanging(var Message: TWMWindowPosChanging);
begin
  inherited;
  if (SWP_NOSIZE and Message.WindowPos.Flags) = 0 then
    CalcMinSize(Message.WindowPos.cx, Message.WindowPos.cy);
end;

procedure TStyledDBNavigator.ClickHandler(Sender: TObject);
begin
  BtnClick(TStyledNavButton(Sender).Index);
end;

procedure TStyledDBNavigator.BtnMouseDown(Sender: TObject; Button: TMouseButton;
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

function TStyledDBNavigator.ApplyDbnavigatorStyle: Boolean;
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

procedure TStyledDBNavigator.ApplyUpdates;
var
  Intf: IDataSetCommandSupport;
begin
  if (Self.DataSource <> nil) and Supports(Self.DataSource.DataSet, IDataSetCommandSupport, Intf) then
    Intf.ExecuteCommand(sApplyUpdatesDataSetCommand, [MaxErrors])
end;

function TStyledDBNavigator.AsVCLStyle: Boolean;
begin
  //if StyleFamily is Classic and StyleElements contains seClient
  //assume to draw the component as the equivalent VCL
  Result := (StyleFamily = DEFAULT_CLASSIC_FAMILY) and
    (seClient in StyleElements);
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

procedure TStyledDBNavigator.CancelUpdates;
var
  Intf: IDataSetCommandSupport;
begin
  if (Self.DataSource <> nil) and Supports(Self.DataSource.DataSet, IDataSetCommandSupport, Intf) then
    Intf.ExecuteCommand(sCancelUpdatesDataSetCommand, [MaxErrors])
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

procedure TStyledDBNavigator.BtnClick(Index: TNavigateBtn);
begin
  if (DataSource <> nil) and (DataSource.State <> dsInactive) then
  begin
    if not (csDesigning in ComponentState) and Assigned(FBeforeAction) then
      FBeforeAction(Self, Index);
    with DataSource.DataSet do
    begin
      case Index of
        nbPrior: Prior;
        nbNext: Next;
        nbFirst: First;
        nbLast: Last;
        nbInsert: Insert;
        nbEdit: Edit;
        nbCancel: Cancel;
        nbPost: Post;
        nbRefresh: Refresh;
        nbDelete:
          if not FConfirmDelete or
            (StyledMessageDlg(SDeleteRecordQuestion, mtConfirmation,
            mbOKCancel, 0) <> idCancel) then Delete;
        nbApplyUpdates: Self.ApplyUpdates;
        nbCancelUpdates: Self.CancelUpdates;
      end;
    end;
  end;
  if not (csDesigning in ComponentState) and Assigned(FOnNavClick) then
    FOnNavClick(Self, Index);
end;

procedure TStyledDBNavigator.WMSetFocus(var Message: TWMSetFocus);
begin
  FButtons[FocusedButton].Invalidate;
end;

procedure TStyledDBNavigator.WMKillFocus(var Message: TWMKillFocus);
begin
  FButtons[FocusedButton].Invalidate;
end;

procedure TStyledDBNavigator.KeyDown(var Key: Word; Shift: TShiftState);
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

procedure TStyledDBNavigator.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
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
  FButtons[nbFirst].Enabled := UpEnable;
  FButtons[nbPrior].Enabled := UpEnable;
  FButtons[nbNext].Enabled := DnEnable;
  FButtons[nbLast].Enabled := DnEnable;
  FButtons[nbDelete].Enabled := CanModify and
    not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
  FButtons[nbRefresh].Enabled := CanRefresh;
  FButtons[nbApplyUpdates].Enabled := CanModify and Self.CanApplyUpdates;
  FButtons[nbCancelUpdates].Enabled := CanModify and Self.CanCancelUpdates;
end;

procedure TStyledDBNavigator.EditingChanged;
var
  CanModify: Boolean;
begin
  CanModify := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;
  Buttons[nbInsert].Enabled := CanModify;
  Buttons[nbEdit].Enabled := CanModify and not FDataLink.Editing;
  Buttons[nbPost].Enabled := CanModify and FDataLink.Editing;
  Buttons[nbCancel].Enabled := CanModify and FDataLink.Editing;
  Buttons[nbRefresh].Enabled := Enabled and (nbRefresh in VisibleButtons) and FDataLink.Active and FDataLink.DataSet.CanRefresh;
  Buttons[nbApplyUpdates].Enabled := CanModify and (nbApplyUpdates in VisibleButtons) and Self.CanApplyUpdates;
  Buttons[nbCancelUpdates].Enabled := CanModify and (nbCancelUpdates in VisibleButtons) and Self.CanCancelUpdates;
end;

procedure TStyledDBNavigator.ActiveChanged;
var
  I: TNavigateBtn;
begin
  if not (Enabled and FDataLink.Active) then
    for I := Low(FButtons) to High(FButtons) do
      FButtons[I].Enabled := False
  else
  begin
    DataChanged;
    EditingChanged;
  end;
end;

procedure TStyledDBNavigator.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then
    ActiveChanged;
end;

procedure TStyledDBNavigator.SetDataSource(const AValue: TDataSource);
begin
  FDataLink.DataSource := AValue;
  if not (csLoading in ComponentState) then
    ActiveChanged;
  if AValue <> nil then
    AValue.FreeNotification(Self);
end;

procedure TStyledDBNavigator.SetDbNavigatorStyle(
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

procedure TStyledDBNavigator.SetDisabledImages(const AValue: TCustomImageList);
begin
  if FDisabledImages <> AValue then
  begin
    FDisabledImages := AValue;
    DisabledImageListChange(Self);
  end;
end;

function TStyledDBNavigator.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TStyledDBNavigator.Loaded;
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
  ActiveChanged;
end;

{TStyledNavButton}

constructor TStyledNavButton.Create(AOwner: TComponent);
begin
  inherited;
  //ControlStyle := ControlStyle + [csCaptureMouse];
  if AOwner is TStyledDbNavigator then
    FDbNavigator := TStyledDbNavigator(AOwner);
  ImageAlignment := iaTop;
end;

destructor TStyledNavButton.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  inherited Destroy;
end;

function TStyledNavButton.GetCaption: TCaption;
begin
  if Assigned(FDbNavigator) and not FDbNavigator.ShowCaptions then
    Result := ''
  else
    Result := inherited GetCaption;
end;

function TStyledNavButton.IsImageAlignmentStored: Boolean;
begin
  if Assigned(FDbNavigator) then
    Result := FImageAlignment <> iaTop
  else
    Result := True;
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
end;

procedure TStyledNavButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled := False;
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
        iaLeft: inherited ImageMargins.Left := DEFAULT_IMAGE_VMARGIN;
        iaRight: inherited ImageMargins.Right := DEFAULT_IMAGE_VMARGIN;
        iaTop: inherited ImageMargins.Top := DEFAULT_IMAGE_VMARGIN;
        iaBottom: inherited ImageMargins.Bottom := DEFAULT_IMAGE_VMARGIN;
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
  if FNavigator <> nil then FNavigator.EditingChanged;
end;

procedure TStyledNavDataLink.DataSetChanged;
begin
  if FNavigator <> nil then FNavigator.DataChanged;
end;

procedure TStyledNavDataLink.ActiveChanged;
begin
  if FNavigator <> nil then FNavigator.ActiveChanged;
end;

end.
