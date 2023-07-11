{******************************************************************************}
{                                                                              }
{       StyledToolbar: a Toolbar with TStyledToolButtons inside                }
{       Based on TFlowPanel and TStyledGraphicButton                           }
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
unit Vcl.StyledToolbar;

interface

{$INCLUDE StyledComponents.inc}

uses
  Vcl.ImgList
  , System.UITypes
  , Vcl.Graphics
  , Vcl.buttons
  , System.SysUtils
  , System.Classes
  , Vcl.StdCtrls
  , Vcl.Themes
  , Vcl.Controls
  , Vcl.ActnList
  , Winapi.CommCtrl
  , Winapi.Messages
  , Winapi.Windows
  , System.Math
  , System.Contnrs
  , Vcl.StyledButton
  , Vcl.ButtonStylesAttributes
  , Vcl.ExtCtrls
  ;

resourcestring
  ERROR_SETTING_TOOLBAR_STYLE = 'Error setting Toolbar Style: %s/%s/%s not available';

type
  EStyledToolbarError = Exception;

  TStyledToolButtonStyle = (tbsStyledButton, tbsStyledCheck, tbsStyledSeparator,
    tbsStyledDivider, tbsTextButton);

  TStyledToolbar = class;
  TStyledToolButton = class;

  TButtonProc = reference to procedure (Button: TStyledToolButton);

  TStyledToolButton = class(TStyledGraphicButton)
  private
    FAllowAllUp: Boolean;
    FAutoSize: Boolean;
    FDown: Boolean;
    FGrouped: Boolean;
    FMarked: Boolean;
    FStyle: TStyledToolButtonStyle;
    FUpdateCount: Integer;
    FCaption: TCaption;
    FHint: string;
    FEnabled: Boolean;
    FImageAlignment: TImageAlignment;
    function GetIndex: Integer;
    function IsCheckedStored: Boolean;
    function IsWidthStored: Boolean;
    procedure SetDown(AValue: Boolean);
    procedure SetGrouped(AValue: Boolean);
    procedure SetMarked(AValue: Boolean);
    procedure SetStyle(AValue: TStyledToolButtonStyle);
    procedure SetCaption(const AValue: TCaption);
    procedure UpdateButtonContent;
    procedure SetImageAlignment(const AValue: TImageAlignment);
    function IsSeparator: Boolean;
    function GetWidth: Integer;
    procedure SetWidth(const AValue: Integer);
    function GetEnable: Boolean;
    procedure SetEnable(const AValue: Boolean);
    function GetHeight: Integer;
    procedure SetHeight(const AValue: Integer);
    function GetCaption: TCaption;
    function IsImageAlignmentStored: Boolean;
    function GetHint: string;
    procedure SetHint(const AValue: string);
    procedure UpAllPrevButtons(const AIndex: Integer);
    procedure UpAllNextButtons(const AIndex: Integer);
  protected
    FToolBar: TStyledToolBar;
    function GetButtonState: TStyledButtonState; override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAutoSize(AValue: Boolean); override;
    procedure SetToolBar(AToolBar: TStyledToolBar);
    procedure SetParent(AParent: TWinControl); override;
    procedure ValidateContainer(AComponent: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property AllowAllUp: Boolean read FAllowAllUp write FAllowAllUp default False;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property Caption: TCaption read GetCaption write SetCaption;
    property Down: Boolean read FDown write SetDown stored IsCheckedStored default False;
    property Enabled: Boolean read GetEnable write SetEnable default True;
    property Grouped: Boolean read FGrouped write SetGrouped default False;
    property Height: Integer read GetHeight write SetHeight stored False;
    property Hint: string read GetHint write SetHint;
    property ImageAlignment: TImageAlignment read FImageAlignment write SetImageAlignment Stored IsImageAlignmentStored;
    property Index: Integer read GetIndex;
    property Marked: Boolean read FMarked write SetMarked default False;
    property Style: TStyledToolButtonStyle read FStyle write SetStyle default tbsStyledButton;
    property Visible;
    property Width: Integer read GetWidth write SetWidth stored IsWidthStored;
  end;


  TSTBNewButtonEvent = procedure(Sender: TStyledToolbar; AIndex: Integer;
    var AButton: TStyledToolButton) of object;
  TSTBButtonEvent = procedure(Sender: TStyledToolbar; AButton: TStyledGraphicButton) of object;

  TStyledToolbar = class(TFlowPanel)
  private
    //Private variable of Properties
    FTransparent: Boolean;
    FTransparentSet: Boolean;
    FButtonHeight: Integer;
    FButtons: TList;
    FShowCaptions: Boolean;
    FImages: TCustomImageList;
    FButtonWidth: Integer;
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

    FStyleRadius: Integer;
    FStyleDrawType: TStyledButtonDrawType;
    FStyleFamily: TStyledButtonFamily;
    FStyleClass: TStyledButtonClass;
    FStyleAppearance: TStyledButtonAppearance;

    FCustomDrawType: Boolean;
    FStyleApplied: Boolean;
    FDisableAlign: Boolean;

    procedure InsertButton(Control: TControl);
    procedure RemoveButton(Control: TControl);
    procedure SetButtonHeight(const AValue: Integer);
    //procedure HotImageListChange(Sender: TObject);
    procedure SetButtonWidth(const AValue: Integer);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SetCustomizable(const AValue: Boolean);
    procedure SetDisabledImages(const AValue: TCustomImageList);
    procedure SetFlat(const AValue: Boolean);
    procedure SetHideClippedButtons(const AValue: Boolean);
    procedure SetImages(const AValue: TCustomImageList);
    procedure SetIndent(const AValue: Integer);
    procedure SetList(const AValue: Boolean);
    procedure SetShowCaptions(const AValue: Boolean);
    procedure SetTransparent(const AValue: Boolean);

    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMParentFontChanged(var Message: TCMParentFontChanged); message CM_PARENTFONTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;

    procedure ImageListChange(Sender: TObject);
    procedure DisabledImageListChange(Sender: TObject);
    procedure ProcessButtons(AButtonProc: TButtonProc);
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
    procedure SetStyleApplied(const Value: Boolean);
    procedure UpdateButtons;
    function GetIndent: Integer;
    function FindLastButton: TStyledToolButton;
  protected
    procedure ClickButton(Button: TStyledToolButton); dynamic;
    procedure CancelMenu; dynamic;
    procedure GetButtonSize(var AWidth, AHeight: Integer);
    procedure ResizeButtons;
    function GetButton(AIndex: Integer): TStyledToolButton;
    function GetButtonCount: Integer;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
  public
    procedure SetToolbarStyle(const AStyleFamily: TStyledButtonFamily;
      const AStyleClass: TStyledButtonClass;
      const AStyleAppearance: TStyledButtonAppearance); overload;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    constructor CreateStyled(AOwner: TComponent;
      const AFamily: TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance); virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function NewButton(out ANewToolButton: TStyledToolButton;
        const AStyle: TStyledToolButtonStyle = tbsStyledButton): Boolean;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property ButtonCount: Integer read GetButtonCount;
    property Buttons[Index: Integer]: TStyledToolButton read GetButton;
    property StyleApplied: Boolean read FStyleApplied write SetStyleApplied;
  published
    property Align default alTop;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 22;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 23;
    property BevelOuter default bvNone;
    property ShowCaption default False;
    property Customizable: Boolean read FCustomizable write SetCustomizable default False;
    property DisabledImages: TCustomImageList read FDisabledImages write SetDisabledImages;
    property Flat: Boolean read FFlat write SetFlat default True;
    property Height default 32;
    property HideClippedButtons: Boolean read FHideClippedButtons write SetHideClippedButtons default False;
    property Images: TCustomImageList read FImages write SetImages;
    property Indent: Integer read GetIndent write SetIndent default 0;
    property List: Boolean read FList write SetList default False;
    property ShowCaptions: Boolean read FShowCaptions write SetShowCaptions default False;
    property Transparent: Boolean read FTransparent write SetTransparent stored FTransparentSet;

    //StyledComponents Attributes
    property StyleRadius: Integer read FStyleRadius write SetStyleRadius stored IsCustomRadius default DEFAULT_RADIUS;
    property StyleDrawType: TStyledButtonDrawType read FStyleDrawType write SetStyleDrawType stored IsCustomDrawType default btRounded;
    property StyleFamily: TStyledButtonFamily read FStyleFamily write SetStyleFamily stored IsStoredStyleFamily;
    property StyleClass: TStyledButtonClass read FStyleClass write SetStyleClass stored IsStoredStyleClass;
    property StyleAppearance: TStyledButtonAppearance read FStyleAppearance write SetStyleAppearance stored IsStoredStyleAppearance;

    //Event Handlers
    property OnCustomizeNewButton: TSTBNewButtonEvent read FOnCustomizeNewButton write FOnCustomizeNewButton;
    property OnCustomizeAdded: TSTBButtonEvent read FOnCustomizeAdded write FOnCustomizeAdded;
  end;

implementation

uses
  Vcl.Consts
  , Vcl.Forms
  , System.Types
  , Vcl.StandardButtonStyles
  ;

const
  DEFAULT_SEP_WIDTH = 10;

{ TStyledToolButton }

constructor TStyledToolButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if StyleServices.Available then
    ControlStyle := [csSetCaption, csClickEvents]
  else
    ControlStyle := [csCaptureMouse, csSetCaption, csClickEvents];
  FStyle := tbsStyledButton;
  Width := 23;
  Height := 22;
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
  if (Button = mbLeft) and (Style = tbsStyledCheck) then
  begin
    if (FDown and AllowAllUp) or (not FDown) then
      Down := not Down;
  end;
end;

procedure TStyledToolButton.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TStyledToolButton.Click;
begin
  inherited Click;
end;

procedure TStyledToolButton.EndUpdate;
begin
  Dec(FUpdateCount);
end;

function TStyledToolButton.GetButtonState: TStyledButtonState;
begin
  if (Style = tbsStyledCheck) and FDown then
    Result := bsmPressed
  else
    Result := inherited GetButtonState;
end;

function TStyledToolButton.GetCaption: TCaption;
begin
  //do not call inherited
  Result := FCaption;
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

function TStyledToolButton.GetHint: string;
begin
  Result := FHint;
end;

function TStyledToolButton.GetIndex: Integer;
begin
  if FToolBar <> nil then
    Result := FToolBar.FButtons.IndexOf(Self)
  else
    Result := -1;
end;

function TStyledToolButton.GetWidth: Integer;
begin
  Result := inherited Width;
end;

function TStyledToolButton.IsCheckedStored: Boolean;
begin
  Result := (ActionLink = nil) or
    not TGraphicButtonActionLink(ActionLink).IsCheckedLinked;
end;

function TStyledToolButton.IsImageAlignmentStored: Boolean;
begin
  if Assigned(FToolBar) then
    Result := FImageAlignment <> iaTop
  else
    Result := True;
end;

function TStyledToolButton.IsWidthStored: Boolean;
begin
  Result := IsSeparator;
end;

procedure TStyledToolButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

procedure TStyledToolButton.SetAutoSize(AValue: Boolean);
begin
  if AValue <> AutoSize then
  begin
    FAutoSize := AValue;
    if not (csLoading in ComponentState) and (FToolBar <> nil) then
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
  LUpdateToolBar := ((AWidth <> Width) or (AHeight <> Height)) and not
    (csLoading in ComponentState);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if LUpdateToolbar and Assigned(FToolBar) and not IsSeparator then
  begin
    if AHeight <> LHeight then
      FToolBar.ButtonHeight := AHeight;
    if AWidth <> LWidth then
      FToolBar.ButtonWidth := AWidth;
  end;
end;

function TStyledToolButton.IsSeparator: Boolean;
begin
  Result := Style in [tbsStyledSeparator, tbsStyledDivider];
end;

procedure TStyledToolButton.UpdateButtonContent;
begin
  //Updates content of Button based on ShowCaptions and Style
  if Assigned(FToolbar) then
  begin
    if FToolBar.ShowCaptions then
    begin
      inherited Caption := FCaption;
      if not FToolbar.List then
      begin
        inherited ImageAlignment := FImageAlignment;
        inherited ImageMargins.Left := 0;
        inherited ImageMargins.Right := 0;
      end
      else
      begin
        if IsRightToLeft then
        begin
          inherited ImageAlignment := iaRight;
          inherited ImageMargins.Right := 4;
          inherited ImageMargins.Left := 0;
        end
        else
        begin
          inherited ImageAlignment := iaLeft;
          inherited ImageMargins.Left := 4;
          inherited ImageMargins.Right := 0;
        end;
      end;
    end
    else
    begin
      inherited Caption := '';
      inherited ImageAlignment := iaCenter;
    end;
    if IsSeparator then
    begin
      inherited Caption := '';
      inherited Hint := '';
      inherited Enabled := False;
      inherited Images := nil;
    end
    else
    begin
      inherited Hint := FHint;
      inherited Enabled := FEnabled;
      inherited Images := FToolbar.Images;
    end;
  end;
  Invalidate;
end;

procedure TStyledToolButton.SetCaption(const AValue: TCaption);
begin
  if FCaption <> AValue then
  begin
    FCaption := AValue;
    UpdateButtonContent;
  end;
end;

procedure TStyledToolButton.UpAllPrevButtons(const AIndex: Integer);
var
  LPrevBtn: TStyledToolButton;
begin
  if Grouped and Assigned(FToolbar) then
  begin
    LPrevBtn := FToolBar.GetButton(AIndex);
    if Assigned(LPrevBtn) and (LPrevBtn.Style = tbsStyledCheck) and (LPrevBtn.Grouped) then
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
    if Assigned(LNextBtn) and (LNextBtn.Style = tbsStyledCheck) and (LNextBtn.Grouped) then
    begin
      LNextBtn.Down := False;
      LNextBtn.UpAllNextButtons(AIndex+1);
    end;
  end;
end;

procedure TStyledToolButton.SetDown(AValue: Boolean);
begin
  if FDown <> AValue then
  begin
    FDown := AValue;
    if FDown and FGrouped then
    begin
      UpAllPrevButtons(Index-1);
      UpAllNextButtons(Index+1);
    end;
    UpdateButtonContent;
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

procedure TStyledToolButton.SetGrouped(AValue: Boolean);
begin
  FGrouped := AValue;
end;

procedure TStyledToolButton.SetHeight(const AValue: Integer);
begin
  inherited Height := AValue;
end;

procedure TStyledToolButton.SetHint(const AValue: string);
begin
  if FHint <> AValue then
  begin
    FHint := AValue;
    UpdateButtonContent;
  end;
end;

procedure TStyledToolButton.SetWidth(const AValue: Integer);
begin
  inherited Width := AValue;
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

procedure TStyledToolButton.SetStyle(AValue: TStyledToolButtonStyle);
var
  WasSeparator: Boolean;
begin
  WasSeparator := IsSeparator;
  if FStyle <> AValue then
  begin
    FStyle := AValue;
    if IsSeparator <> WasSeparator then
      Width := DEFAULT_SEP_WIDTH;
    UpdateButtonContent;
  end;
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

procedure TStyledToolButton.ValidateContainer(AComponent: TComponent);
var
  W: Integer;
begin
  inherited ValidateContainer(AComponent);
  { Update non-stored Width and Height if inserting into TStyledToolBar }
  if (csLoading in ComponentState) and (AComponent is TStyledToolBar) then
  begin
    if IsSeparator then
      W := Width else
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
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csMenuEvents, csSetCaption, csGestures];
  ShowCaption := False;
  Width := 150;
  Height := 29;
  Align := alTop;
  FButtonWidth := 23;
  FButtonHeight := 22;
  FButtons := TList.Create;

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;

  FDisabledImageChangeLink := TChangeLink.Create;
  FDisabledImageChangeLink.OnChange := DisabledImageListChange;

  //FHotImageChangeLink := TChangeLink.Create;
  //FHotImageChangeLink.OnChange := HotImageListChange;

  { The default value for Transparent now depends on if you have
    Themes turned on or off (this only works on XP) }
  FTransparent := StyleServices.Enabled;
  ParentBackground := True;
  ParentColor := True;
  BevelOuter := bvNone;
  Flat := True;

  FStyleDrawType := btRounded;
  FStyleRadius := DEFAULT_RADIUS;
  FStyleFamily := AFamily;
  FStyleClass := AClass;
  FStyleAppearance := AAppearance;
end;

constructor TStyledToolbar.Create(AOwner: TComponent);
begin
  CreateStyled(AOwner,
    DEFAULT_CLASSIC_FAMILY,
    DEFAULT_WINDOWS_CLASS,
    DEFAULT_APPEARANCE);
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

procedure TStyledToolbar.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
end;

procedure TStyledToolbar.SetButtonHeight(const AValue: Integer);
begin
  if FButtonHeight <> AValue then
  begin
    FButtonHeight := AValue;
    ResizeButtons;
  end;
end;

procedure TStyledToolbar.SetButtonWidth(const AValue: Integer);
begin
  if FButtonWidth <> AValue then
  begin
    FButtonWidth := AValue;
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
    LButton.FToolBar := Self;
    LButton.Height := FButtonHeight;
    if not LButton.IsSeparator then
      LButton.Width := FButtonWidth;
    FButtons.Insert(FButtons.Count, LButton);
    if Assigned(FOnCustomizeAdded) then
      FOnCustomizeAdded(Self, LButton);
  end;
end;

function TStyledToolbar.IsCustomDrawType: Boolean;
begin
  Result := FCustomDrawType;
end;

function TStyledToolbar.IsCustomRadius: Boolean;
begin
  Result := StyleRadius <> DEFAULT_RADIUS;
end;

function TStyledToolbar.IsStoredStyleAppearance: Boolean;
var
  LClass: TStyledButtonClass;
  LAppearance: TStyledButtonAppearance;
begin
  StyleFamilyCheckAttributes(FStyleFamily, LClass, LAppearance);
  Result := FStyleAppearance <> LAppearance;
end;

function TStyledToolbar.IsStoredStyleClass: Boolean;
var
  LClass: TStyledButtonClass;
  LAppearance: TStyledButtonAppearance;
begin
  StyleFamilyCheckAttributes(FStyleFamily, LClass, LAppearance);
  Result := FStyleClass <> LClass;
end;

function TStyledToolbar.IsStoredStyleFamily: Boolean;
begin
  Result := FStyleFamily <> DEFAULT_CLASSIC_FAMILY;
end;

function TStyledToolbar.FindLastButton: TStyledToolButton;
var
  LLastRect: TRect;
  LLastButton: TStyledToolButton;
begin
  LLastRect.Top := 0;
  LLastRect.Left := 0;
  LLastRect.Width := 0;
  LLastRect.Height := 0;
  LLastButton := nil;
  ProcessButtons(
    procedure (ABtn: TStyledToolButton)
    begin
      if (ABtn.Left > LLastRect.Left+LLastRect.Width) or
        (ABtn.Top > LLastRect.Top+LLastRect.Height) then
      begin
        LLastButton := ABtn;
        LLastRect := LLastButton.BoundsRect;
      end;
    end);
  Result := LLastButton;
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

procedure TStyledToolbar.SetDisabledImages(const AValue: TCustomImageList);
begin
  if FDisabledImages <> AValue then
  begin
    FDisabledImages := AValue;
    DisabledImageListChange(Self);
  end;
end;

procedure TStyledToolbar.SetFlat(const AValue: Boolean);
begin
  FFlat := AValue;
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

procedure TStyledToolbar.SetList(const AValue: Boolean);
begin
  if FList <> AValue then
  begin
    FList := AValue;
    UpdateButtons;
  end;
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
  if (AControl is TStyledToolButton) and not FDisableAlign then
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
begin
  Result := StyleFamilyCheckAttributes(FStyleFamily,
    FStyleClass, FStyleAppearance);
  if Result or (csDesigning in ComponentState) then
  begin
    (* TODO with custom Styled
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
    *)
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

procedure TStyledToolbar.SetStyleApplied(const Value: Boolean);
begin
  FStyleApplied := Value;
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
  end;
end;

procedure TStyledToolbar.SetStyleDrawType(const AValue: TStyledButtonDrawType);
begin
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
    RecreateWnd;
  end;
  FTransparentSet := True;
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

function TStyledToolbar.GetIndent: Integer;
begin
  if AlignWithMargins then
    Result := Margins.Left
  else
    Result := 0;
end;

procedure TStyledToolbar.ResizeButtons;
var
  LWasAutoSize: Boolean;
begin
  FDisableAlign := True;
  try
    if (FButtonHeight <> 0) and (FButtonWidth <> 0) and
      (FUpdateCount = 0) then
    begin
      LWasAutoSize := AutoSize;
      AutoSize := False;
      try
        BeginUpdate;
        ProcessButtons(
          procedure (ABtn: TStyledToolButton)
          begin
            if not ABtn.IsSeparator then
              ABtn.Width := FButtonWidth;
            ABtn.Height := FButtonHeight;
          end
        );
      finally
        AutoSize := LWasAutoSize;
        EndUpdate;
      end;
    end;
  finally
    FDisableAlign := False;
  end;
end;

function TStyledToolbar.GetButton(AIndex: Integer): TStyledToolButton;
begin
  if (AIndex >= 0) and (AIndex <= FButtons.Count-1) then
    Result := TStyledToolButton(FButtons[AIndex])
  else
    Result := nil;
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
    LButton := TStyledToolButton(FButtons.Items[I]);
    AButtonProc(LButton);
  end;
end;

function TStyledToolbar.NewButton(out ANewToolButton: TStyledToolButton;
  const AStyle: TStyledToolButtonStyle = tbsStyledButton): Boolean;
var
  LastButton: TStyledToolButton;
begin
  Result := False;
  FDisableAlign := True;
  try
    if Assigned(FOnCustomizeNewButton) then
    begin
      FOnCustomizeNewButton(Self,
        FButtons.Count, ANewToolButton);
      Result := Assigned(ANewToolButton);
    end;
    if not Result then
    begin
      ANewToolButton := TStyledToolButton.Create(Self.Owner);
      ANewToolButton.Style := AStyle;
      ANewToolButton.Parent := Self;
      ANewToolButton.FToolbar := Self;
      ANewToolButton.SetButtonStyle(FStyleFamily, FStyleClass, FStyleAppearance);
      LastButton := FindLastButton;
      if Assigned(LastButton) then
      begin
        ANewToolButton.Left := LastButton.Left+LastButton.Width;
        ANewToolButton.Top := LastButton.Top+LastButton.Height;
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
    FDisableAlign := False;
  end;
end;

end.
