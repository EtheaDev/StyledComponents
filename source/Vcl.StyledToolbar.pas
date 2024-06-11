{******************************************************************************}
{                                                                              }
{  StyledToolbar: a Toolbar with TStyledToolButtons inside                     }
{  Based on TFlowPanel and TStyledGraphicButton                                }
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
unit Vcl.StyledToolbar;

interface

{$INCLUDE StyledComponents.inc}

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
  , Vcl.Graphics
  , Vcl.GraphUtil
  , Winapi.Messages
  , Winapi.Windows
  , Vcl.StyledButton
  , Vcl.ButtonStylesAttributes
  , Vcl.StandardButtonStyles
  ;

resourcestring
  ERROR_SETTING_TOOLBAR_STYLE = 'Error setting Toolbar Style: %s/%s/%s not available';

type
  EStyledToolbarError = Exception;

  TStyledToolbar = class;
  TStyledToolButton = class;
  TStyledToolButtonClass = class of TStyledToolButton;

  TButtonProc = reference to procedure (Button: TStyledToolButton);
  TControlProc = reference to procedure (Control: TControl);

  { TStyledToolButton }
  TStyledToolButton = class(TCustomStyledGraphicButton)
  private
    FAutoSize: Boolean;
    FGrouped: Boolean;
    FMarked: Boolean;
    FStyle: TToolButtonStyle;
    FEnabled: Boolean;
    FImageAlignment: TImageAlignment;
    FMenuItem: TMenuItem;
    function IsStoredCursor: Boolean;
    function IsStoredFlat: Boolean;
    function IsCustomRadius: Boolean;
    function IsCustomRoundedCorners: Boolean;
    function IsCustomDrawType: Boolean;
    function IsStoredStyleFamily: Boolean;
    function IsStoredStyleAppearance: Boolean;
    function GetIndex: Integer;
    function IsImagesStored: Boolean;
    function IsWidthStored: Boolean;
    procedure SetGrouped(AValue: Boolean);
    procedure SetMarked(AValue: Boolean);
    procedure SetStyle(AValue: TToolButtonStyle);
    procedure UpdateButtonContent;
    procedure SetImageAlignment(const AValue: TImageAlignment);
    function IsSeparator: Boolean;
    function IsDropDown: Boolean;
    function GetEnable: Boolean;
    procedure SetEnable(const AValue: Boolean);
    function GetHeight: Integer;
    procedure SetHeight(const AValue: Integer);
    function IsImageAlignmentStored: Boolean;
    procedure UpAllPrevButtons(const AIndex: Integer);
    procedure UpAllNextButtons(const AIndex: Integer);
    procedure SetMenuItem(const AValue: TMenuItem);
    function GetWrap: Boolean;
    procedure SetWrap(const AValue: Boolean);
    procedure UpdateGroupIndex;
    function GetStyleDrawType: TStyledButtonDrawType;
    procedure SetStyleDrawType(const AValue: TStyledButtonDrawType);
  protected
    FToolBar: TStyledToolBar;
    function GetCaptionToDraw: TCaption; override;
    procedure SetCaption(const AValue: TCaption); override;
    function IsStoredStyleClass: Boolean; override;
    function IsEnabledStored: Boolean; override;
    function IsCaptionStored: Boolean; override;
    function GetImage(out AImageList: TCustomImageList;
      out AImageIndex: Integer): Boolean; override;
    function GetButtonState: TStyledButtonState; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure SetAutoSize(AValue: Boolean); override;
    procedure SetToolBar(AToolBar: TStyledToolBar);
    procedure SetParent(AParent: TWinControl); override;
    procedure ValidateContainer(AComponent: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    //property ActiveStyleName;
    property Action;
    property Align;
    property AllowAllUp default False;
    property Anchors;
    property AsVCLComponent stored False;
    property Constraints;
    property Cursor stored IsStoredCursor;
    property Down default False;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled: Boolean read GetEnable write SetEnable stored IsEnabledStored;
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
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    {$IFDEF D10_4+}
    property StyleName;
    {$ENDIF}
    property StyleElements;
    property Transparent;
    property Visible;
    property Caption: TCaption read GetCaption write SetCaption stored IsCaptionStored;
    property CaptionAlignment;
    property CommandLinkHint;
    property ImageAlignment: TImageAlignment read FImageAlignment write SetImageAlignment Stored IsImageAlignmentStored;
    property DisabledImageIndex;
    property DisabledImages;
    property DropDownMenu;
    property Flat stored IsStoredFlat;
    property Glyph;
    property NumGlyphs;
    property HotImageIndex;
    property Images stored IsImagesStored;
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
    property Tag;
    //StyledComponents Attributes
    property StyleRadius stored IsCustomRadius;
    property StyleRoundedCorners stored IsCustomRoundedCorners;
    property StyleDrawType: TStyledButtonDrawType read GetStyleDrawType write SetStyleDrawType stored IsCustomDrawType;
    property StyleFamily stored IsStoredStyleFamily;
    property StyleClass stored IsStoredStyleClass;
    property StyleAppearance stored IsStoredStyleAppearance;
    property WordWrap stored False;
    property ButtonStyleNormal;
    property ButtonStylePressed;
    property ButtonStyleSelected;
    property ButtonStyleHot;
    property ButtonStyleDisabled;
    property NotificationBadge;
    property OnDropDownClick;

    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property Grouped: Boolean read FGrouped write SetGrouped default False;
    property Height: Integer read GetHeight write SetHeight stored False;
    property Index: Integer read GetIndex;
    property Marked: Boolean read FMarked write SetMarked default False;
    property MenuItem: TMenuItem read FMenuItem write SetMenuItem;
    property Style: TToolButtonStyle read FStyle write SetStyle default tbsButton;
    property Width stored IsWidthStored;
    property Wrap: Boolean read GetWrap write SetWrap default False;
  end;


  TSTBNewButtonEvent = procedure(Sender: TStyledToolbar; AIndex: Integer;
    var AButton: TStyledToolButton) of object;
  TSTBButtonEvent = procedure(Sender: TStyledToolbar;
    AButton: TCustomStyledGraphicButton) of object;

  { TStyledToolbar }
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TStyledToolbar = class(TCustomFlowPanel)
  private
    //Private variable of Properties
    FTransparent: Boolean;
    FButtons: TList;
    FShowCaptions: Boolean;
    FImages: TCustomImageList;
    FButtonWidth: Integer;
    FButtonHeight: Integer;
    FFlat: Boolean;
    FHideClippedButtons: Boolean;
    FCustomizable: Boolean;
    FList: Boolean;
    FDisabledImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FDisabledImageChangeLink: TChangeLink;
    FUpdateCount: Integer;
    FOnCustomizeNewButton: TSTBNewButtonEvent;
    FOnCustomizeAdded: TSTBButtonEvent;
    FCaptureChangeCancels: Boolean;
    FInMenuLoop: Boolean;
    FAutoSize: Boolean;
    FButtonsCursor: TCursor;

    //Properties ignores (only for backward compatibility)
    FGradientDrawingOptions: TTBGradientDrawingOptions;
    FGradientDirection: TGradientDirection;
    FDrawingStyle: TTBDrawingStyle;
    FGradientEndColor: TColor;
    FGradientStartColor: TColor;

    //Styled Attributes
    FStyleRadius: Integer;
    FStyleDrawType: TStyledButtonDrawType;
    FStyleFamily: TStyledButtonFamily;
    FStyleClass: TStyledButtonClass;
    FStyleAppearance: TStyledButtonAppearance;
    FCustomDrawType: Boolean;
    FStyleApplied: Boolean;
    FDisableButtonAlign: Integer;
    FOnToolButtonClick: TNotifyEvent;

    class var
    _DefaultStyleDrawType: TStyledButtonDrawType;
    _UseCustomDrawType: Boolean;
    _DefaultFamily: TStyledButtonFamily;
    _DefaultClass: TStyledButtonClass;
    _DefaultAppearance: TStyledButtonAppearance;
    _DefaultStyleRadius: Integer;
    _DefaultButtonsCursor: TCursor;

    function ControlsWidth: Integer;
    function ControlsHeight: Integer;
    procedure InsertButton(Control: TControl);
    procedure RemoveButton(Control: TControl);
    procedure SetButtonHeight(const AValue: Integer);
    //procedure HotImageListChange(Sender: TObject);
    procedure SetButtonWidth(const AValue: Integer);
    procedure SetCustomizable(const AValue: Boolean);
    procedure SetDisabledImages(const AValue: TCustomImageList);
    procedure SetFlat(const AValue: Boolean);
    procedure SetHideClippedButtons(const AValue: Boolean);
    procedure SetImages(const AValue: TCustomImageList);
    procedure SetIndent(const AValue: Integer);
    procedure SetList(const AValue: Boolean);
    procedure SetShowCaptions(const AValue: Boolean);

    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMParentFontChanged(var Message: TCMParentFontChanged); message CM_PARENTFONTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;

    function FindButtonFromAccel(Accel: Word): TStyledToolButton;
    procedure ImageListChange(Sender: TObject);
    procedure DisabledImageListChange(Sender: TObject);
    procedure ProcessButtons(AButtonProc: TButtonProc);
    procedure ProcessControls(AControlProc: TControlProc);
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
    function ApplyToolbarStyle: Boolean;
    procedure SetStyleApplied(const AValue: Boolean);
    procedure UpdateButtons;
    function GetIndent: Integer;
    function FindLastControl: TControl;
    function GetWrapable: Boolean;
    procedure SetWrapable(const AValue: Boolean);
    function GetEdgeBorders: TEdgeBorders;
    function GetEdgeInner: TEdgeStyle;
    function GetEdgeOuter: TEdgeStyle;
    procedure SetEdgeBorders(const AValue: TEdgeBorders);
    procedure SetEdgeInner(const AValue: TEdgeStyle);
    procedure SetEdgeOuter(const AValue: TEdgeStyle);
    procedure UpdateBevelKind;
    function GetActiveStyleName: string;
    function AsVCLStyle: Boolean;
    function GetAsVCLComponent: Boolean;
    procedure SetAsVCLComponent(const AValue: Boolean);
    function GetAutoWrap: Boolean;
    function GetAutoSize: Boolean;
    procedure SetTransparent(const AValue: Boolean);
    function GetDisableButtonAlign: Boolean;
    procedure SetDisableButtonAlign(const AValue: Boolean);
    function IsGradientEndColorStored: Boolean;
    procedure SetButtonsCursor(const AValue: TCursor);
    property DisableButtonAlign: Boolean read GetDisableButtonAlign write SetDisableButtonAlign;
  protected
    {$IFDEF D10_1+}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ENDIF}
    procedure SetAutoSize(AValue: Boolean); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function TrackMenu(Button: TStyledToolButton): Boolean; dynamic;
    procedure SetParent(AParent: TWinControl); override;
    procedure UpdateStyleElements; override;
    procedure ClickButton(Button: TStyledToolButton); dynamic;
    procedure CancelMenu; dynamic;
    procedure GetButtonSize(var AWidth, AHeight: Integer);
    procedure ResizeButtons;
    function GetButton(AIndex: Integer): TStyledToolButton;
    function GetButtonCount: Integer;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure AdjustSize; override;
    function GetStyledToolButtonClass: TStyledToolButtonClass; virtual;
    procedure Loaded; override;
  public
    procedure Assign(Source: TPersistent); override;
    class procedure RegisterDefaultRenderingStyle(
      const ADrawType: TStyledButtonDrawType;
      const AFamily: TStyledButtonFamily = DEFAULT_CLASSIC_FAMILY;
      const AClass: TStyledButtonClass = DEFAULT_WINDOWS_CLASS;
      const AAppearance: TStyledButtonAppearance = DEFAULT_APPEARANCE;
      const AStyleRadius: Integer = DEFAULT_RADIUS); virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure ClearButtons;
    procedure Click; override;
    procedure SetToolbarStyle(const AStyleFamily: TStyledButtonFamily;
      const AStyleClass: TStyledButtonClass;
      const AStyleAppearance: TStyledButtonAppearance);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    //Styled constructor
    constructor CreateStyled(AOwner: TComponent;
      const AFamily: TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance); virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function NewButton(out ANewToolButton: TStyledToolButton;
        const AStyle: TToolButtonStyle = tbsButton): Boolean;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property AsVCLComponent: Boolean read GetAsVCLComponent write SetAsVCLComponent stored False;
    property ButtonCount: Integer read GetButtonCount;
    property Buttons[Index: Integer]: TStyledToolButton read GetButton;
    property StyleApplied: Boolean read FStyleApplied write SetStyleApplied;
    property AutoWrap: Boolean read GetAutoWrap;
  published
    property Align default alTop;
    property Anchors;
    property AutoSize: Boolean read GetAutoSize write SetAutoSize default False;
    property ButtonsCursor: TCursor read FButtonsCursor write SetButtonsCursor default DEFAULT_CURSOR;
    property BorderWidth;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 22;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 23;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property Customizable: Boolean read FCustomizable write SetCustomizable default False;
    property DisabledImages: TCustomImageList read FDisabledImages write SetDisabledImages;
    property DoubleBuffered;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EdgeBorders: TEdgeBorders read GetEdgeBorders write SetEdgeBorders default [];
    property EdgeInner: TEdgeStyle read GetEdgeInner write SetEdgeInner default esNone;
    property EdgeOuter: TEdgeStyle read GetEdgeOuter write SetEdgeOuter default esNone;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default True;
    property Font;
    property Height;
    property HideClippedButtons: Boolean read FHideClippedButtons write SetHideClippedButtons default False;
    property Images: TCustomImageList read FImages write SetImages;
    property Indent: Integer read GetIndent write SetIndent default 0;
    property List: Boolean read FList write SetList default False;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor default True;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCaptions: Boolean read FShowCaptions write SetShowCaptions default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property Transparent: Boolean read FTransparent write SetTransparent stored False;
    property Visible;
    property StyleElements;
    property Wrapable: Boolean read GetWrapable write SetWrapable default True;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;

    //Properties ignores (only for backward compatibility)
    property GradientDrawingOptions: TTBGradientDrawingOptions read FGradientDrawingOptions write FGradientDrawingOptions default [gdoHotTrack, gdoGradient];
    property GradientDirection: TGradientDirection read FGradientDirection write FGradientDirection default gdVertical;
    property DrawingStyle: TTBDrawingStyle read FDrawingStyle write FDrawingStyle default TTBDrawingStyle.dsNormal;
    property GradientEndColor: TColor read FGradientEndColor write FGradientEndColor stored IsGradientEndColorStored;
    property GradientStartColor: TColor read FGradientStartColor write FGradientStartColor default clWindow;

    //StyledComponents Attributes
    property StyleRadius: Integer read FStyleRadius write SetStyleRadius stored IsCustomRadius;
    property StyleDrawType: TStyledButtonDrawType read FStyleDrawType write SetStyleDrawType stored IsCustomDrawType;
    property StyleFamily: TStyledButtonFamily read FStyleFamily write SetStyleFamily stored IsStoredStyleFamily;
    property StyleClass: TStyledButtonClass read FStyleClass write SetStyleClass stored IsStoredStyleClass;
    property StyleAppearance: TStyledButtonAppearance read FStyleAppearance write SetStyleAppearance stored IsStoredStyleAppearance;

    //Event Handlers
    property OnCustomizeNewButton: TSTBNewButtonEvent read FOnCustomizeNewButton write FOnCustomizeNewButton;
    property OnCustomizeAdded: TSTBButtonEvent read FOnCustomizeAdded write FOnCustomizeAdded;
    property OnToolButtonClick: TNotifyEvent read FOnToolButtonClick write FOnToolButtonClick;
  end;

implementation

uses
  Vcl.Consts
  , Vcl.Forms
  , System.Types
  , System.RTLConsts
  ;

const
  DEFAULT_SEP_WIDTH = 6;
  DEFAULT_TOOLBUTTON_WIDTH = 23;
  DEFAULT_TOOLBUTTON_HEIGHT = 22;
  DEFAULT_IMAGE_HMARGIN = 8;

{ TStyledToolButton }

constructor TStyledToolButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if StyleServices.Available then
    ControlStyle := [csSetCaption, csClickEvents]
  else
    ControlStyle := [csCaptureMouse, csSetCaption, csClickEvents];
  FStyle := tbsButton;
  Width := DEFAULT_TOOLBUTTON_WIDTH;
  Height := DEFAULT_TOOLBUTTON_HEIGHT;
  ImageAlignment := iaTop;
  FEnabled := True;
end;

procedure TStyledToolButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TStyledToolButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
(*
  if (Button = mbLeft) and (Style = tbsCheck) then
  begin
    if (Down and AllowAllUp) or (not Down) then
      Down := not Down;
  end;
*)
end;

procedure TStyledToolButton.Click;
begin
  if not IsDropDown and Assigned(DropDownMenu) then
    ShowDropDownMenu
  else
  begin
    inherited Click;
    if Assigned(FToolbar) and Assigned(FToolbar.FOnToolButtonClick) then
    begin
      FToolbar.FOnToolButtonClick(Self);
    end;
  end;
end;

function TStyledToolButton.GetButtonState: TStyledButtonState;
begin
  if (Style = tbsCheck) and Down then
    Result := bsmPressed
  else
    Result := inherited GetButtonState;
end;

function TStyledToolButton.GetCaptionToDraw: TCaption;
begin
  if Assigned(FToolBar) and not FToolBar.ShowCaptions then
    Result := ''
  else
    Result := inherited GetCaption;
end;

function TStyledToolButton.GetEnable: Boolean;
begin
  if IsSeparator then
    Result := False
  else
    Result := inherited Enabled;
end;

function TStyledToolButton.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function TStyledToolButton.GetImage(out AImageList: TCustomImageList;
  out AImageIndex: Integer): Boolean;
begin
  if IsSeparator then
  begin
    AImageList := nil;
    AImageIndex := -1;
    Result := False;
  end
  else
    Result := inherited GetImage(AImageList, AImageIndex);
end;

function TStyledToolButton.GetIndex: Integer;
begin
  if Assigned(FToolBar) then
    Result := FToolBar.FButtons.IndexOf(Self)
  else
    Result := -1;
end;

function TStyledToolButton.GetStyleDrawType: TStyledButtonDrawType;
begin
  Result := inherited StyleDrawType;
end;

function TStyledToolButton.GetWrap: Boolean;
begin
  Result := inherited WordWrap;
end;

function TStyledToolButton.IsCaptionStored: Boolean;
begin
  if IsSeparator then
    Result := False
  else
    Result := inherited IsCaptionStored;
end;

function TStyledToolButton.IsImageAlignmentStored: Boolean;
begin
  if Assigned(FToolBar) then
    Result := FImageAlignment <> iaTop
  else
    Result := True;
end;

function TStyledToolButton.IsImagesStored: Boolean;
begin
  Result := not Assigned(FToolBar) or
    (Images <> FToolBar.Images);
end;

function TStyledToolButton.IsWidthStored: Boolean;
begin
  if IsSeparator then
  begin
    Result := Width <> DEFAULT_SEP_WIDTH
  end
  else if Assigned(FToolBar) then
  begin
    if IsDropDown then
      Result := (Width - GetSplitButtonWidth <> FToolbar.ButtonWidth)
    else
      Result := (Width  <> FToolbar.ButtonWidth);
  end
  else
    Result := Width <> DEFAULT_TOOLBUTTON_WIDTH;
end;

procedure TStyledToolButton.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if AOperation = opRemove then
  begin
    if AComponent = FToolBar then
      FToolBar := nil;
  end;
end;

procedure TStyledToolButton.SetAutoSize(AValue: Boolean);
begin
  if AValue <> AutoSize then
  begin
    FAutoSize := AValue;
    if Assigned(FToolBar) then
      FToolBar.ResizeButtons;
  end;
end;

procedure TStyledToolButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  LUpdateToolBar: Boolean;
  LHeight, LWidth: Integer;
begin
  LHeight := Height;
  LWidth := Width;
  if IsDropDown then
    LWidth := FToolBar.ButtonWidth + GetSplitButtonWidth;
  LUpdateToolBar := (not RescalingButton)
    and Assigned(FToolBar) and ((AWidth <> Width) or (AHeight <> Height))
    and not IsSeparator
    and not (csLoading in ComponentState);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if LUpdateToolbar then
  begin
    LUpdateToolbar := False;
    if AHeight <> LHeight then
    begin
      FToolBar.FButtonHeight := AHeight;
      LUpdateToolbar := True;
    end;
    if AWidth <> LWidth then
    begin
      FToolBar.FButtonWidth := AWidth;
      LUpdateToolbar := True;
    end;
    if LUpdateToolbar and (AWidth <> 0) and (AHeight <> 0) then
      FToolBar.ResizeButtons;
  end;
end;

function TStyledToolButton.IsSeparator: Boolean;
begin
  Result := Style in [tbsSeparator, tbsDivider];
end;

function TStyledToolButton.IsDropDown: Boolean;
begin
  Result := Style = tbsDropDown;
end;

function TStyledToolButton.IsEnabledStored: Boolean;
begin
  if IsSeparator then
    Result := False
  else
    Result := inherited IsEnabledStored;
end;

procedure TStyledToolButton.UpdateButtonContent;
begin
  //Updates content of Button based on ShowCaptions and Style
  if Assigned(FToolbar) then
  begin
    if FToolBar.ShowCaptions then
    begin
      if not FToolbar.List then
      begin
        inherited ImageAlignment := FImageAlignment;
        //inherited ImageMargins.Left := 0;
        //inherited ImageMargins.Right := 0;
      end
      else
      begin
        if IsRightToLeft then
        begin
          inherited ImageAlignment := iaRight;
          //inherited ImageMargins.Right := DEFAULT_IMAGE_HMARGIN;
          //inherited ImageMargins.Left := 0;
        end
        else
        begin
          inherited ImageAlignment := iaLeft;
          //inherited ImageMargins.Left := DEFAULT_IMAGE_HMARGIN;
          //inherited ImageMargins.Right := 0;
        end;
      end;
    end
    else
    begin
      inherited ImageAlignment := iaCenter;
    end;
    if IsSeparator then
    begin
      inherited Enabled := False;
      inherited DisabledImages := nil;
      inherited Images := nil;
      inherited StyleRadius := 1;
    end
    else
    begin
      inherited Enabled := FEnabled;
      inherited DisabledImages := FToolbar.DisabledImages;
      inherited Images := FToolbar.Images;
    end;
  end;
  Invalidate;
end;

procedure TStyledToolButton.SetCaption(const AValue: TCaption);
begin
  if AValue <> (inherited Caption) then
  begin
    inherited SetCaption(AValue);
    Invalidate;
  end;
end;

procedure TStyledToolButton.UpAllPrevButtons(const AIndex: Integer);
var
  LPrevBtn: TStyledToolButton;
begin
  if Grouped and Assigned(FToolbar) then
  begin
    LPrevBtn := FToolBar.GetButton(AIndex);
    if Assigned(LPrevBtn) and (LPrevBtn.Style = tbsCheck) and (LPrevBtn.Grouped) then
    begin
      LPrevBtn.Down := False;
      LPrevBtn.UpAllPrevButtons(AIndex-1);
    end;
  end;
end;

procedure TStyledToolButton.UpAllNextButtons(const AIndex: Integer);
var
  LNextBtn: TStyledToolButton;
begin
  if Grouped and Assigned(FToolbar) then
  begin
    LNextBtn := FToolBar.GetButton(AIndex);
    if Assigned(LNextBtn) and (LNextBtn.Style = tbsCheck) and (LNextBtn.Grouped) then
    begin
      LNextBtn.Down := False;
      LNextBtn.UpAllNextButtons(AIndex+1);
    end;
  end;
end;

procedure TStyledToolButton.SetEnable(const AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    UpdateButtonContent;
  end;
end;

procedure TStyledToolButton.UpdateGroupIndex;
begin
  if Style = tbsCheck then
  begin
    if FGrouped then
      Render.GroupIndex := -1
    else if Render.GroupIndex = -1 then
      Render.GroupIndex := 0;
  end;
end;

procedure TStyledToolButton.SetGrouped(AValue: Boolean);
begin
  if FGrouped <> AValue then
  begin
    FGrouped := AValue;
    UpdateGroupIndex;
  end;
end;

procedure TStyledToolButton.SetHeight(const AValue: Integer);
begin
  inherited Height := AValue;
end;

procedure TStyledToolButton.SetImageAlignment(const AValue: TImageAlignment);
begin
  if FImageAlignment <> AValue then
  begin
    FImageAlignment := AValue;
    UpdateButtonContent;
  end;
end;

procedure TStyledToolButton.SetMarked(AValue: Boolean);
begin
  FMarked := AValue;
end;

procedure TStyledToolButton.SetMenuItem(const AValue: TMenuItem);
begin
  { Copy all appropriate values from menu item }
  if AValue <> nil then
  begin
    if FMenuItem <> AValue then
      AValue.FreeNotification(Self);
    Action := AValue.Action;
    Caption := AValue.Caption;
    Down := AValue.Checked;
    Enabled := AValue.Enabled;
    Hint := AValue.Hint;
    ImageIndex := AValue.ImageIndex;
{$IFDEF D10_4+}
    ImageName := AValue.ImageName;
{$ENDIF}
    Visible := AValue.Visible;
  end;
  FMenuItem := AValue;
end;

function TStyledToolButton.IsCustomRadius: boolean;
begin
  Result := not IsSeparator;
  if Result then
  begin
    if Assigned(FToolBar) then
      Result := StyleRadius <> FToolBar.StyleRadius
    else
      Result := StyleRadius <> DEFAULT_RADIUS;
  end;
end;

function TStyledToolButton.IsCustomRoundedCorners: Boolean;
begin
  Result := StyleRoundedCorners <> ALL_ROUNDED_CORNERS;
end;

function TStyledToolButton.IsCustomDrawType: Boolean;
begin
  if Assigned(FToolBar) then
    Result := StyleDrawType <> FToolBar.StyleDrawType
  else
    Result := StyleDrawType <> btRoundRect;
end;

function TStyledToolButton.IsStoredStyleFamily: Boolean;
begin
  if Assigned(FToolBar) then
    Result := not SameText(StyleFamily,FToolBar.StyleFamily)
  else
    Result := True;
end;

function TStyledToolButton.IsStoredStyleClass: Boolean;
begin
  if Assigned(FToolBar) then
    Result := not SameText(StyleClass,FToolBar.StyleClass)
  else
    Result := inherited IsStoredStyleClass;
end;

function TStyledToolButton.IsStoredFlat: Boolean;
begin
  if Assigned(FToolBar) then
    Result := Flat <> FToolBar.Flat
  else
    Result := True;
end;

function TStyledToolButton.IsStoredCursor: Boolean;
begin
  if Assigned(FToolBar) then
    Result := Cursor <> FToolBar.FButtonsCursor
  else
    Result := Cursor <> DEFAULT_CURSOR;
end;

function TStyledToolButton.IsStoredStyleAppearance: Boolean;
begin
  if Assigned(FToolBar) then
    Result := not SameText(StyleAppearance, FToolBar.StyleAppearance)
  else
    Result := True;
end;

procedure TStyledToolButton.SetParent(AParent: TWinControl);
{$IFDEF D10_4+}
var
  LToolBar: TStyledToolBar;
{$ENDIF}
begin
  inherited;
  {$IFDEF D10_4+}
  if (AParent <> nil) and (AParent is TStyledToolBar) then
  begin
    LToolBar := TStyledToolBar(AParent);
    if (LToolBar.Images <> nil) and LToolBar.Images.IsImageNameAvailable then
      if (ImageIndex < 0) and (ImageName <> '') then
        ImageIndex := LToolBar.Images.GetIndexByName(ImageName)
      else
      if (ImageIndex >= 0) and (ImageName = '') then
        ImageName := LToolBar.Images.GetNameByIndex(ImageIndex);
  end;
  {$ENDIF}
end;

procedure TStyledToolButton.SetStyle(AValue: TToolButtonStyle);
var
  WasSeparator: Boolean;
begin
  if AValue = tbsTextButton then
    raise Exception.Create('tbsTextButton not available');
  WasSeparator := IsSeparator;
  if FStyle <> AValue then
  begin
    FStyle := AValue;
    UpdateGroupIndex;
    if IsDropDown then
    begin
      inherited Style := TCustomButton.TButtonStyle.bsSplitButton;
      if FToolBar.AutoSize then
        FToolBar.ResizeButtons;
    end
    else
    begin
      inherited Style := TCustomButton.TButtonStyle.bsPushButton;
    end;
    if IsSeparator <> WasSeparator then
    begin
      Width := DEFAULT_SEP_WIDTH;
      StyleDrawType := StyleDrawType;
    end;
    UpdateButtonContent;
  end;
end;

procedure TStyledToolButton.SetStyleDrawType(
  const AValue: TStyledButtonDrawType);
begin
  if not IsSeparator then
    inherited StyleDrawType := AValue;
end;

procedure TStyledToolButton.SetToolBar(AToolBar: TStyledToolBar);
begin
  if FToolBar <> AToolBar then
  begin
    if FToolBar <> nil then
      FToolBar.RemoveButton(Self);
    Parent := AToolBar;
    if AToolBar <> nil then
      AToolBar.InsertButton(Self);
  end;
end;

procedure TStyledToolButton.SetWrap(const AValue: Boolean);
begin
  inherited WordWrap := AValue;
end;

procedure TStyledToolButton.ValidateContainer(AComponent: TComponent);
var
  W: Integer;
begin
  inherited ValidateContainer(AComponent);
  { Update non-stored Width and Height if inserting into TStyledToolBar }
  if (csLoading in ComponentState) and (AComponent is TStyledToolBar) then
  begin
    if IsSeparator then
      W := DEFAULT_SEP_WIDTH else
      W := TStyledToolBar(AComponent).ButtonWidth;
    SetBounds(Left, Top, W, TStyledToolBar(AComponent).ButtonHeight);
  end;
end;

{ TStyledButtonToolbar }

procedure TStyledToolbar.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TStyledToolbar.CancelMenu;
begin
  if FInMenuLoop then
  begin
    MouseCapture := False;
  end;
  FInMenuLoop := False;
  FCaptureChangeCancels := False;
end;

{$IFDEF D10_1+}
procedure TStyledToolbar.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  FButtonWidth := MulDiv(FButtonWidth, M, D);
  FButtonHeight := MulDiv(FButtonHeight, M, D);
  inherited ChangeScale(M, D, isDpiChange);
end;
{$ENDIF}


procedure TStyledToolbar.ClearButtons;
var
  LButton: TStyledToolButton;
  LIndex: Integer;
begin
  while FButtons.Count > 0 do
  begin
    LIndex := FButtons.Count-1;
    LButton := GetButton(LIndex);
    LButton.Free;
    FButtons.Delete(LIndex);
  end;
end;

procedure TStyledToolbar.Click;
begin
  inherited;
  ;
end;

procedure TStyledToolbar.ClickButton(Button: TStyledToolButton);
var
  P: TPoint;
  SmallPt: TSmallPoint;
begin
  FCaptureChangeCancels := False;
  P := Button.ClientToScreen(Point(0, 0));
  SmallPt := PointToSmallPoint(ScreenToClient(P));
  with SmallPt do
    PostMessage(Handle, WM_LBUTTONDOWN, MK_LBUTTON, MakeLong(X, Y));
end;

constructor TStyledToolbar.CreateStyled(AOwner: TComponent;
  const AFamily: TStyledButtonFamily; const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance);
begin
  inherited Create(AOwner);
  FButtons := TList.Create;

  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csMenuEvents, csSetCaption, csGestures];
  ShowCaption := False;
  Width := 150;
  Height := 29;
  FButtonWidth := 23;
  FButtonHeight := 22;
  FButtonsCursor := DEFAULT_CURSOR;
  BevelKind := bkNone;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  BevelEdges := [];

  Transparent := StyleServices.Enabled;
  ParentBackground := True;
  ParentColor := True;
  BevelOuter := bvNone;
  Flat := True;

  Align := alTop;

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;

  FDisabledImageChangeLink := TChangeLink.Create;
  FDisabledImageChangeLink.OnChange := DisabledImageListChange;

  //FHotImageChangeLink := TChangeLink.Create;
  //FHotImageChangeLink.OnChange := HotImageListChange;

  FGradientDrawingOptions := [gdoHotTrack,gdoGradient];
  FGradientDirection := gdVertical;
  FDrawingStyle := TTBDrawingStyle.dsNormal;
  FGradientEndColor := GetShadowColor(clBtnFace, -25);
  FGradientStartColor := clWindow;
  FStyleDrawType := _DefaultStyleDrawType;
  FStyleRadius := _DefaultStyleRadius;
  FStyleFamily := AFamily;
  FStyleClass := AClass;
  FStyleAppearance := AAppearance;
  FButtonsCursor := _DefaultButtonsCursor;
end;

constructor TStyledToolbar.Create(AOwner: TComponent);
begin
  CreateStyled(AOwner,
    _DefaultFamily,
    _DefaultClass,
    _DefaultAppearance);
end;

destructor TStyledToolbar.Destroy;
begin
  //FHotImageChangeLink.Free;
  FreeAndNil(FDisabledImageChangeLink);
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FButtons);
  inherited Destroy;
end;

procedure TStyledToolbar.DisabledImageListChange(Sender: TObject);
begin
  ProcessButtons(
    procedure (ABtn: TStyledToolButton)
    begin
      ABtn.DisabledImages := DisabledImages;
    end
  );
end;

procedure TStyledToolbar.EndUpdate;
begin
  Dec(FUpdateCount);
end;

procedure TStyledToolbar.ImageListChange(Sender: TObject);
begin
  ProcessButtons(
    procedure (ABtn: TStyledToolButton)
    begin
      ABtn.Images := Images;
    end
  );
end;

procedure TStyledToolbar.SetButtonsCursor(const AValue: TCursor);
begin
  if FButtonsCursor <> AValue then
  begin
    FButtonsCursor := AValue;
    ProcessButtons(
      procedure (ABtn: TStyledToolButton)
      begin
        ABtn.Cursor := AValue;
      end
    );
  end;
end;

procedure TStyledToolbar.AdjustSize;
var
  LSize: Integer;
begin
  inherited;
  if not (csLoading in ComponentState) and HandleAllocated then
  begin
    //Recalc correct Size only for AutoSize and AutoWrap
    if Assigned(Parent) and FAutoSize and AutoWrap and (FButtons.Count > 0) then
    begin
      if (Align in [alLeft, alRight]) then
      begin
        LSize := ControlsWidth;
        if LSize > Parent.ClientWidth then
          LSize := Parent.ClientWidth;
        if (LSize <> Width) then
          Width := LSize;
      end
      else if Align in [alTop, alBottom] then
      begin
        LSize := ControlsHeight;
        if LSize > Parent.ClientHeight then
          LSize := Parent.ClientHeight;
        if LSize <> Height then
          Height := LSize;
      end;
    end;
  end;
end;

procedure TStyledToolbar.SetWrapable(const AValue: Boolean);
begin
  if AutoWrap <> AValue then
  begin
    if AValue then
    begin
      //FlowPanel AutoSize don't work correctly when AutoWrap is True
      inherited AutoSize := False;
    end;
    inherited AutoWrap := AValue;
  end;
end;

procedure TStyledToolbar.SetAutoSize(AValue: Boolean);
begin
  if AValue <> FAutoSize then
  begin
    FAutoSize := AValue;
    //FlowPanel AutoSize don't work correctly when AutoWrap is True
    if not AutoWrap then
      inherited AutoSize := AValue
    else
      RequestAlign;
  end;
end;

procedure TStyledToolbar.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
end;

procedure TStyledToolbar.SetButtonHeight(const AValue: Integer);
begin
  if FButtonHeight <> AValue then
  begin
    FButtonHeight := AValue;
    if AutoSize and (Align in [alTop, alBottom]) and (Height < AValue) then
      Height := AValue;
    ResizeButtons;
  end;
end;

procedure TStyledToolbar.SetButtonWidth(const AValue: Integer);
begin
  if FButtonWidth <> AValue then
  begin
    FButtonWidth := AValue;
    if AutoSize and (Align in [alLeft, alRight]) and (Width < AValue) then
      Width := AValue;
    ResizeButtons;
  end;
end;

procedure TStyledToolBar.InsertButton(Control: TControl);
var
  LButton: TStyledToolButton;
begin
  if Control is TStyledToolButton then
  begin
    LButton := TStyledToolButton(Control);
    LButton.BeginUpdate;
    try
      LButton.FToolBar := Self;
      LButton.Images := Self.Images;
      LButton.StyleRadius := FStyleRadius;
      LButton.StyleDrawType := FStyleDrawType;
      LButton.StyleFamily := FStyleFamily;
      LButton.StyleClass := FStyleClass;
      LButton.StyleAppearance := FStyleAppearance;
      LButton.StyleElements := StyleElements;
      LButton.Cursor := FButtonsCursor;
      LButton.RescalingButton := True;
      try
        LButton.Height := FButtonHeight;
        LButton.Flat := FFlat;
        if not LButton.IsSeparator then
          LButton.Width := FButtonWidth;
        FButtons.Insert(FButtons.Count, LButton);
        if Assigned(FOnCustomizeAdded) then
          FOnCustomizeAdded(Self, LButton);
      finally
        LButton.RescalingButton := False;
      end;
    finally
      LButton.EndUpdate;
    end;
    LButton.UpdateButtonContent;
  end
  else
  begin
    FButtons.Insert(FButtons.Count, Control);
  end;
  if Showing then
    AdjustSize;
end;

function TStyledToolbar.IsCustomDrawType: Boolean;
begin
  Result := FCustomDrawType;
end;

function TStyledToolbar.IsCustomRadius: Boolean;
begin
  Result := StyleRadius <> DEFAULT_RADIUS;
end;

function TStyledToolbar.IsGradientEndColorStored: Boolean;
begin
  Result := FGradientEndColor <> GetShadowColor(clBtnFace, -25);
end;

function TStyledToolbar.IsStoredStyleAppearance: Boolean;
var
  LClass: TStyledButtonClass;
  LAppearance: TStyledButtonAppearance;
  LButtonFamily: TButtonFamily;
begin
  StyleFamilyCheckAttributes(FStyleFamily, LClass, LAppearance, LButtonFamily);
  Result := FStyleAppearance <> LAppearance;
end;

function TStyledToolbar.IsStoredStyleClass: Boolean;
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

function TStyledToolbar.IsStoredStyleFamily: Boolean;
begin
  Result := FStyleFamily <> DEFAULT_CLASSIC_FAMILY;
end;

procedure TStyledToolbar.Loaded;
begin
  inherited;
  ResizeButtons;
end;

function TStyledToolbar.FindButtonFromAccel(Accel: Word): TStyledToolButton;
var
  I: Integer;
begin
  for I := 0 to FButtons.Count - 1 do
    if TControl(FButtons[I]) is TStyledToolButton then
    begin
      Result := TStyledToolButton(FButtons[I]);
      if Result.Visible and Result.Enabled and IsAccel(Accel, Result.Caption) then
        Exit;
    end;
  Result := nil;
end;

function TStyledToolbar.FindLastControl: TControl;
var
  LLastRect: TRect;
  LLastControl: TControl;
begin
  LLastRect.Top := 0;
  LLastRect.Left := 0;
  LLastRect.Width := 0;
  LLastRect.Height := 0;
  LLastControl := nil;
  ProcessControls(
    procedure (AControl: TControl)
    begin
      if (AControl.Left > LLastRect.Left+LLastRect.Width) or
        (AControl.Top > LLastRect.Top+LLastRect.Height) then
      begin
        LLastControl := AControl;
        LLastRect := LLastControl.BoundsRect;
      end;
    end);
  Result := LLastControl;
end;

class procedure TStyledToolbar.RegisterDefaultRenderingStyle(
  const ADrawType: TStyledButtonDrawType; const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass; const AAppearance: TStyledButtonAppearance;
  const AStyleRadius: Integer);
begin
  _DefaultStyleDrawType := ADrawType;
  _UseCustomDrawType := True;
  _DefaultFamily := AFamily;
  _DefaultClass := AClass;
  _DefaultAppearance := AAppearance;
  _DefaultStyleRadius := AStyleRadius;
end;

procedure TStyledToolBar.RemoveButton(Control: TControl);
var
  I: Integer;
begin
  I := FButtons.IndexOf(Control);
  if I >= 0 then
  begin
    if Control is TStyledToolButton then
    begin
      TStyledToolButton(Control).FToolBar := nil;
      FButtons.Remove(Control);
    end;
  end;
  if Showing then
    AdjustSize;
end;

procedure TStyledToolbar.SetCustomizable(const AValue: Boolean);
begin
  FCustomizable := AValue;
end;

procedure TStyledToolbar.SetImages(const AValue: TCustomImageList);
begin
  if FImages <> AValue then
  begin
    FImages := AValue;
    ImageListChange(Self);
  end;
end;

procedure TStyledToolbar.SetDisableButtonAlign(const AValue: Boolean);
begin
  if AValue then
    Inc(FDisableButtonAlign)
  else
    Dec(FDisableButtonAlign);
end;

procedure TStyledToolbar.SetDisabledImages(const AValue: TCustomImageList);
begin
  if FDisabledImages <> AValue then
  begin
    FDisabledImages := AValue;
    DisabledImageListChange(Self);
  end;
end;

procedure TStyledToolbar.SetEdgeBorders(const AValue: TEdgeBorders);
var
  LBevelEdges: TBevelEdges;
begin
  LBevelEdges := [];
  if ebLeft in AValue then
    LBevelEdges := LBevelEdges + [beLeft];
  if ebTop in AValue then
    LBevelEdges := LBevelEdges + [beTop];
  if ebRight in AValue then
    LBevelEdges := LBevelEdges + [beRight];
  if ebBottom in AValue then
    LBevelEdges := LBevelEdges + [beBottom];

  inherited BevelEdges := LBevelEdges;
  UpdateBevelKind;
end;

procedure TStyledToolbar.UpdateBevelKind;
begin
  if (BevelOuter <> bvNone) or (BevelInner <> bvNone) or
    (BevelEdges <> []) then
    BevelKind := bkFlat
  else
    BevelKind := bkNone;
end;

procedure TStyledToolbar.SetEdgeInner(const AValue: TEdgeStyle);
begin
  case AValue of
    esNone: inherited BevelInner := bvNone;
    esRaised: inherited BevelInner := bvRaised;
    esLowered: inherited BevelInner := bvLowered;
  end;
  if BevelInner <> bvNone then
    BevelKind := bkSoft
  else
    BevelKind := bkNone;
  UpdateBevelKind;
end;

procedure TStyledToolbar.SetEdgeOuter(const AValue: TEdgeStyle);
begin
  case AValue of
    esNone: inherited BevelOuter := bvNone;
    esRaised: inherited BevelOuter := bvRaised;
    esLowered: inherited BevelOuter := bvLowered;
  end;
  UpdateBevelKind;
end;

procedure TStyledToolbar.SetFlat(const AValue: Boolean);
begin
  if FFlat <> AValue then
  begin
    FFlat := AValue;
    ProcessButtons(
      procedure (ABtn: TStyledToolButton)
      begin
        ABtn.Flat := AValue;
      end);
  end;
end;

procedure TStyledToolbar.SetHideClippedButtons(const AValue: Boolean);
begin
  FHideClippedButtons := AValue;
end;

procedure TStyledToolbar.SetIndent(const AValue: Integer);
begin
  if AValue <> 0 then
  begin
    AlignWithMargins := True;
    Margins.Left := AValue;
  end;
end;

procedure TStyledToolbar.UpdateButtons;
begin
  ProcessButtons(
    procedure (ABtn: TStyledToolButton)
    begin
      ABtn.UpdateButtonContent;
    end);
end;

procedure TStyledToolbar.Assign(Source: TPersistent);
var
  LToolbar: TStyledToolbar;
begin
  inherited Assign(Source);
  if Source is TStyledToolbar then
  begin
    LToolbar := TStyledToolbar(Source);
    FTransparent := LToolbar.FTransparent;
    FShowCaptions := LToolbar.FShowCaptions;
    FButtonsCursor := LToolbar.FButtonsCursor;
    FButtonHeight :=  LToolbar.FButtonHeight;
    FButtonWidth := LToolbar.FButtonWidth;
    FCustomizable := LToolbar.FCustomizable;
    DisabledImages := LToolbar.FDisabledImages;
    FFlat := LToolbar.FFlat;
    FHideClippedButtons := LToolbar.FHideClippedButtons;
    Images := LToolbar.FImages;
    FList := LToolbar.FList;
    FStyleFamily := LToolbar.FStyleFamily;
    FStyleClass := LToolbar.FStyleClass;
    FStyleAppearance := LToolbar.FStyleAppearance;
    FStyleRadius := LToolbar.FStyleRadius;
    FStyleDrawType := LToolbar.FStyleDrawType;
    Invalidate;
  end;
end;

function TStyledToolbar.AsVCLStyle: Boolean;
begin
  Result := (StyleFamily = DEFAULT_CLASSIC_FAMILY) and
    (seClient in StyleElements);
end;

function TStyledToolbar.GetAsVCLComponent: Boolean;
begin
  Result := (StyleFamily = DEFAULT_CLASSIC_FAMILY) and
    (seClient in StyleElements) and
    (FStyleClass = GetActiveStyleName);
end;

procedure TStyledToolbar.SetAsVCLComponent(const AValue: Boolean);
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

procedure TStyledToolbar.UpdateStyleElements;
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
    StyleApplied := ApplyToolbarStyle;
  ProcessButtons(
    procedure (ABtn: TStyledToolButton)
    begin
      ABtn.UpdateStyleElements;
      ABtn.StyleDrawType := Self.StyleDrawType;
    end);
  end;
  inherited;
end;

procedure TStyledToolbar.WMSysCommand(var Message: TWMSysCommand);

  function IsMenuBar: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to FButtons.Count - 1 do
      if (TControl(FButtons[I]) is TStyledToolButton)
      and Assigned(TStyledToolButton(FButtons[I]).MenuItem) then
      begin
        Result := True;
        Break;
      end;
  end;

var
  Button: TStyledToolButton;
begin
  { Enter menu loop if only the Alt key is pressed -- ignore Alt-Space and let
    the default processing show the system menu. }
  if not FInMenuLoop and Enabled and Showing and (ShowCaptions) and IsMenuBar then
    with Message do
      if (CmdType and $FFF0 = SC_KEYMENU) and (Key <> VK_SPACE) and
        (Key <> Word('-')) and (GetCapture = 0) then
      begin
        if Key = 0 then
          Button := nil else
          Button := FindButtonFromAccel(Key);
        if (Key = 0) or ((Button <> nil) and (Button.ImageIndex > -1)) then
        begin
          TrackMenu(Button);
          Result := 1;
          Exit;
        end;
      end;
end;

procedure TStyledToolbar.SetList(const AValue: Boolean);
begin
  if FList <> AValue then
  begin
    FList := AValue;
    UpdateButtons;
  end;
end;

procedure TStyledToolbar.SetParent(AParent: TWinControl);
begin
  inherited;
  if Assigned(Self.Parent) then
    UpdateStyleElements;
end;

procedure TStyledToolbar.SetShowCaptions(const AValue: Boolean);
begin
  if FShowCaptions <> AValue then
  begin
    FShowCaptions := AValue;
    UpdateButtons;
  end;
end;

procedure TStyledToolbar.AlignControls(AControl: TControl; var Rect: TRect);
var
  LPoint: TPoint;
  LRect: TRect;
  I: Integer;
  LButton, LSourceButton, LTargetButton: TStyledToolButton;
begin
  if (AControl is TStyledToolButton) and not DisableButtonAlign then
  begin
    //Move Button selected in new position
    LSourceButton := TStyledToolButton(AControl);
    LPoint.Y := AControl.Top + (AControl.Height div 2);
    LPoint.X := AControl.Left + (AControl.Width div 2);
    LTargetButton := nil;
    for I := 0 to FButtons.Count -1 do
    begin
      LButton := TStyledToolButton(FButtons[I]);
      LRect := LButton.BoundsRect;
      if (LButton <> LSourceButton) and PtInRect(LRect, LPoint) then
      begin
        LTargetButton := LButton;
        Break;
      end;
    end;
    if Assigned(LTargetButton) then
    begin
      //Replace index of Source Control to Target
      SetControlIndex(AControl, GetControlIndex(LTargetButton));
    end;
  end;
  inherited;
end;

function TStyledToolbar.ApplyToolbarStyle: Boolean;
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
    finally
      LAttributesNormal.Free;
      LAttributesOther.Free;
    end;
  end;
end;

procedure TStyledToolbar.SetStyleAppearance(
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
      procedure (ABtn: TStyledToolButton)
      begin
        if ABtn.StyleAppearance = StyleAppearance then
          ABtn.StyleAppearance := LValue;
      end);
    FStyleAppearance := LValue;
    StyleApplied := ApplyToolbarStyle;
  end;
end;

procedure TStyledToolbar.SetStyleApplied(const AValue: Boolean);
begin
  FStyleApplied := AValue;
end;

procedure TStyledToolbar.SetStyleClass(const AValue: TStyledButtonClass);
var
  LValue: TStyledButtonClass;
begin
  LValue := AValue;
  if LValue = '' then
    LValue := DEFAULT_WINDOWS_CLASS;
  if (LValue <> Self.FStyleClass) or not FStyleApplied then
  begin
    ProcessButtons(
      procedure (ABtn: TStyledToolButton)
      begin
        if ABtn.StyleClass = StyleClass then
          ABtn.StyleClass := LValue;
      end);
    FStyleClass := LValue;
    StyleApplied := ApplyToolbarStyle;
    if (FStyleFamily = DEFAULT_CLASSIC_FAMILY) and
      (LValue <> 'Windows') then
      StyleElements := [seFont, seBorder];
  end;
end;

procedure TStyledToolbar.SetStyleDrawType(const AValue: TStyledButtonDrawType);
begin
  FCustomDrawType := True;
  if FStyleDrawType <> AValue then
  begin
    ProcessButtons(
      procedure (ABtn: TStyledToolButton)
      begin
        if ABtn.StyleDrawType = StyleDrawType then
          ABtn.StyleDrawType := AValue;
      end);
    FStyleDrawType := AValue;
    StyleApplied := ApplyToolbarStyle;
  end;
end;

procedure TStyledToolbar.SetStyleFamily(const AValue: TStyledButtonFamily);
var
  LValue: TStyledButtonFamily;
begin
  LValue := AValue;
  if LValue = '' then
    LValue := DEFAULT_CLASSIC_FAMILY;
  if (LValue <> Self.FStyleFamily) or not FStyleApplied then
  begin
    ProcessButtons(
      procedure (ABtn: TStyledToolButton)
      begin
        if ABtn.StyleFamily = FStyleFamily then
          ABtn.StyleFamily := LValue;
      end);
    FStyleFamily := LValue;
    StyleApplied := ApplyToolbarStyle;
  end;
  if FStyleFamily = DEFAULT_CLASSIC_FAMILY then
    StyleElements := [seFont, seClient, seBorder];
end;

procedure TStyledToolbar.SetStyleRadius(const AValue: Integer);
begin
  if FStyleRadius <> AValue then
  begin
    if AValue <= 0 then
      raise EReadError.create(SInvalidProperty);
    ProcessButtons(
      procedure (ABtn: TStyledToolButton)
      begin
        if ABtn.StyleRadius = FStyleRadius then
          ABtn.StyleRadius := AValue;
      end);
    FStyleRadius := AValue;
    StyleApplied := ApplyToolbarStyle;
  end;
end;

procedure TStyledToolbar.SetToolbarStyle(
  const AStyleFamily: TStyledButtonFamily;
  const AStyleClass: TStyledButtonClass;
  const AStyleAppearance: TStyledButtonAppearance);
begin
  StyleFamily := AStyleFamily;
  StyleClass := AStyleClass;
  StyleAppearance := AStyleAppearance;
  if not ApplyToolbarStyle then
    raise EStyledButtonError.CreateFmt(ERROR_SETTING_TOOLBAR_STYLE,
      [AStyleFamily, AStyleClass, AStyleAppearance]);
end;

procedure TStyledToolbar.SetTransparent(const AValue: Boolean);
begin
  if FTransparent <> AValue then
  begin
    FTransparent := AValue;
    if AValue then
      ControlStyle := ControlStyle - [csOpaque] else
      ControlStyle := ControlStyle + [csOpaque];
  end;
end;

function TStyledToolbar.TrackMenu(Button: TStyledToolButton): Boolean;
begin
  { Already in menu loop - click button to drop-down menu }
  if FInMenuLoop then
  begin
    if Button <> nil then
    begin
      ClickButton(Button);
      Result := True;
    end
    else
      Result := False;
    Exit;
  end
  else
    Result := False;
end;

procedure TStyledToolbar.GetButtonSize(var AWidth, AHeight: Integer);
var
  LWidth, LHeight: Integer;
begin
  LWidth := AWidth;
  LHeight := AHeight;
  ProcessButtons(
    procedure (ABtn: TStyledToolButton)
    begin
      LWidth := Max(LWidth, ABtn.Width);
      LHeight := Max(LHeight, ABtn.Height);
    end
  );
  AWidth := LWidth;
  AHeight := LHeight;
end;

procedure TStyledToolbar.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  Control: TControl;
begin
  for I := 0 to FButtons.Count - 1 do
    Proc(TStyledButton(FButtons[I]));
  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    if (Control.Owner = Root) and (FButtons.IndexOf(Control) = -1) then
      Proc(Control);
  end;
end;

function TStyledToolbar.GetDisableButtonAlign: Boolean;
begin
  Result := FDisableButtonAlign > 0;
end;

function TStyledToolbar.GetEdgeBorders: TEdgeBorders;

begin
  Result := [];
  if beLeft in BevelEdges then
    Result := Result + [ebLeft];
  if beTop in BevelEdges then
    Result := Result + [ebTop];
  if beTop in BevelEdges then
    Result := Result + [ebTop];
  if beRight in BevelEdges then
    Result := Result + [ebRight];
  if beBottom in BevelEdges then
    Result := Result + [ebBottom];
end;

function TStyledToolbar.GetEdgeInner: TEdgeStyle;
begin
  case BevelInner of
    bvNone: Result := esNone;
    bvLowered: Result := esLowered;
    bvRaised: Result := esRaised;
  else
    Result := esNone;
  end;
end;

function TStyledToolbar.GetEdgeOuter: TEdgeStyle;
begin
  case BevelOuter of
    bvNone: Result := esNone;
    bvLowered: Result := esLowered;
    bvRaised: Result := esRaised;
  else
    Result := esNone;
  end;
end;

function TStyledToolbar.GetIndent: Integer;
begin
  if AlignWithMargins then
    Result := Margins.Left
  else
    Result := 0;
end;

function TStyledToolbar.GetWrapable: Boolean;
begin
  Result := inherited AutoWrap;
end;

procedure TStyledToolbar.ResizeButtons;
begin
  if (csLoading in ComponentState) then
    Exit;

  DisableButtonAlign := True;
  try
    if (FButtonHeight <> 0) and (FButtonWidth <> 0) and
      (FUpdateCount = 0) then
    begin
      BeginUpdate;
      try
        ProcessButtons(
          procedure (ABtn: TStyledToolButton)
          begin
            ABtn.BeginUpdate;
            try
              if not ABtn.IsSeparator then
              begin
                if ABtn.IsDropDown then
                  ABtn.Width := FButtonWidth + ABtn.GetSplitButtonWidth
                else
                  ABtn.Width := FButtonWidth;
              end;
              ABtn.Height := FButtonHeight;
            finally
              ABtn.EndUpdate;
            end;
          end
        );
      finally
        EndUpdate;
      end;
    end;
  finally
    DisableButtonAlign := False;
  end;
end;

function TStyledToolbar.GetActiveStyleName: string;
begin
  Result := Vcl.ButtonStylesAttributes.GetActiveStyleName(Self);
end;

function TStyledToolbar.GetAutoSize: Boolean;
begin
  //FlowPanel AutoSize don't work correctly when AutoWrap is True
  if not AutoWrap then
    Result := inherited AutoSize
  else
    Result := FAutoSize;
end;

function TStyledToolbar.GetAutoWrap: Boolean;
begin
  Result := inherited AutoWrap;
end;

function TStyledToolbar.GetButton(AIndex: Integer): TStyledToolButton;
var
  LObject: TObject;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex <= FButtons.Count-1) then
  begin
    LObject := FButtons[AIndex];
    if LObject is TStyledToolButton then
      Result := TStyledToolButton(LObject)
  end;
end;

function TStyledToolbar.GetButtonCount: Integer;
begin
  if Assigned(FButtons) then
    Result := FButtons.Count
  else
    Result := 0;
end;

procedure TStyledToolBar.CMControlChange(var Message: TCMControlChange);
begin
  inherited;
  with Message do
    if Inserting then
      InsertButton(Control)
    else
      RemoveButton(Control);
end;

procedure TStyledToolbar.CMFontChanged(var Message: TMessage);
begin
  inherited;
  //Apply the Font to every buttons
  ProcessButtons(
    procedure (ABtn: TStyledToolButton)
    begin
      ABtn.Font.Assign(Font);
    end
  );
end;

procedure TStyledToolbar.CMParentFontChanged(var Message: TCMParentFontChanged);
begin
  inherited;
  //Reset the Font of every buttons to Parent Font (of Toolbar)
  if ParentFont then
    ProcessButtons(
      procedure (ABtn: TStyledToolButton)
      begin
        ABtn.ParentFont := True;
      end
    );
end;

procedure TStyledToolbar.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  UpdateStyleElements;
  Invalidate;
end;

function TStyledToolbar.ControlsHeight: Integer;
var
  LSize: Integer;
begin
  if AlignWithMargins then
    LSize := Margins.Top + Margins.Bottom + 1
  else
    LSize := 1;
  ProcessControls(
    procedure (AControl: TControl)
    begin
      if AControl.Height > LSize then
        LSize := LSize + AControl.Height;
    end);
  Result := LSize;
end;

function TStyledToolbar.ControlsWidth: Integer;
var
  LSize: Integer;
begin
  if AlignWithMargins then
    LSize := Margins.Left + Margins.Right + 1
  else
    LSize := 1;
  ProcessControls(
    procedure (AControl: TControl)
    begin
      if AControl.Width > LSize then
        LSize := AControl.Width;
    end);
  Result := LSize;
end;

procedure TStyledToolbar.ProcessButtons(
  AButtonProc: TButtonProc);
var
  I: Integer;
  LButton: TStyledToolButton;
begin
  if not Assigned(FButtons) then
    Exit;
  for I := 0 to FButtons.Count -1 do
  begin
    if TObject(FButtons.Items[I]) is TStyledToolButton then
    begin
      LButton := TStyledToolButton(FButtons.Items[I]);
      AButtonProc(LButton);
    end;
  end;
end;

procedure TStyledToolbar.ProcessControls(
  AControlProc: TControlProc);
var
  I: Integer;
  LControl: TControl;
begin
  if not Assigned(FButtons) then
    Exit;
  for I := 0 to FButtons.Count -1 do
  begin
    if TObject(FButtons.Items[I]) is TControl then
    begin
      LControl := TControl(FButtons.Items[I]);
      AControlProc(Lcontrol);
    end;
  end;
end;

function TStyledToolbar.GetStyledToolButtonClass: TStyledToolButtonClass;
begin
  Result := TStyledToolButton;
end;

function TStyledToolbar.NewButton(out ANewToolButton: TStyledToolButton;
  const AStyle: TToolButtonStyle = tbsButton): Boolean;
var
  LLastControl: TControl;
begin
  Result := False;
  DisableButtonAlign := True;
  try
    if Assigned(FOnCustomizeNewButton) then
    begin
      FOnCustomizeNewButton(Self,
        FButtons.Count, ANewToolButton);
      Result := Assigned(ANewToolButton);
    end;
    if not Result then
    begin
      ANewToolButton := GetStyledToolButtonClass.Create(Self.Owner);
      ANewToolButton.Style := AStyle;
      ANewToolButton.Parent := Self;
      ANewToolButton.FToolbar := Self;
      ANewToolButton.SetButtonStyle(FStyleFamily, FStyleClass, FStyleAppearance);
      LLastControl := FindLastControl;
      if Assigned(LLastControl) then
      begin
        ANewToolButton.Left := LLastControl.Left+LLastControl.Width;
        ANewToolButton.Top := LLastControl.Top+LLastControl.Height;
        if LLastControl is TStyledToolButton then
          ANewToolButton.ImageIndex := TStyledToolButton(LLastControl).ImageIndex + 1;
      end
      else
      begin
        ANewToolButton.Left := 0;
        ANewToolButton.Top := 0;
      end;
      if Assigned(ANewToolButton) then
      begin
        if not ANewToolButton.IsSeparator then
          ANewToolButton.Width := FButtonWidth;
        ANewToolButton.Height := FButtonHeight;
        Result := True;
      end;
    end;
  finally
    DisableButtonAlign := False;
  end;
end;

procedure TStyledToolbar.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if AOperation = opRemove then
  begin
    if AComponent = Images then
      Images := nil;
    if AComponent = DisabledImages then
      DisabledImages := nil;
  end;
end;

initialization
  TStyledToolbar._DefaultStyleDrawType := DEFAULT_STYLEDRAWTYPE;
  TStyledToolbar._DefaultFamily := DEFAULT_CLASSIC_FAMILY;
  TStyledToolbar._DefaultClass := DEFAULT_WINDOWS_CLASS;
  TStyledToolbar._DefaultAppearance := DEFAULT_APPEARANCE;
  TStyledToolbar._DefaultStyleRadius := DEFAULT_RADIUS;
  TStyledToolbar._DefaultButtonsCursor := DEFAULT_CURSOR;

end.
