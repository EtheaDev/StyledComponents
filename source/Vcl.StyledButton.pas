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
unit Vcl.StyledButton;

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
  , Winapi.CommCtrl
  , Winapi.Messages
  , Winapi.Windows
  , System.Math
  , Vcl.ButtonStylesAttributes;

const
  StyledButtonsVersion = '0.9.5';
  DEFAULT_BTN_WIDTH = 75;
  DEFAULT_BTN_HEIGHT = 25;
type
  TStyledButtonMode = (bsmNormal, bsmDown, bsmFocused, bsmHot, bsmDisabled);
  TStyledGraphicButton = class;
  TStyledButton = class;

  TButtonFocusControl = class(TWinControl)
  private
    FButton: TStyledButton;
  protected
    procedure WMKeyUp(var Message: TWMKeyDown); message WM_KEYUP;
    procedure WndProc(var Message: TMessage); override;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
  public
    constructor Create(AOwner: TComponent; AStyledButton: TStyledButton); reintroduce;
    property TabOrder;
    property TabStop default True;
  end;

  TGraphicButtonActionLink = class(TControlActionLink)
  protected
    FClient: TStyledGraphicButton;
    function IsImageIndexLinked: Boolean; override;
    {$IFDEF D10_4+}
    function IsImageNameLinked: Boolean; override;
    {$ENDIF}
    procedure SetImageIndex(Value: Integer); override;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
  end;

  TStyledGraphicButton = class(TGraphicControl)
  private
    FButtonStyleNormal: TStyledButtonAttributes;
    FButtonStyleDown: TStyledButtonAttributes;
    FButtonStyleFocused: TStyledButtonAttributes;
    FButtonStyleHot: TStyledButtonAttributes;
    FButtonStyleDisabled: TStyledButtonAttributes;
    FModalResult: TModalResult;
    FMouseInControl: Boolean;
    FState: TButtonState;
    FPen: TPen;
    FBrush: TBrush;
    FImageMargins: TImageMargins;
    FRadius: Integer;
    FBorderType: TBtnBorder;
    FStyleFamily: TStyledButtonFamily;
    FStyleClass: TStyledButtonClass;
    FStyleAppearance: TStyledButtonAppearance;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FImageIndex: TImageIndex;
    {$IFDEF D10_4+}
    FImageName: TImageName;
    {$ENDIF}
    FImageAlignment: TImageAlignment;
    FTag: Integer;
    function IsPressed: Boolean;
    function IsDefault: Boolean;
    procedure SetImageMargins(const AValue: TImageMargins);
    function IsRadius: Boolean;
    procedure SetRadius(const AValue: Integer);
    function IconAssigned: Boolean;
    function GetPictureWidth: Integer;
    function GetPictureHeight: Integer;
    function IsStoredStyleClass: Boolean;
    function IsStoredStyleFamily: Boolean;
    function IsStoredStyleAppearance: Boolean;
    procedure SetStyleFamily(const AValue: TStyledButtonFamily);
    procedure SetStyleClass(const AValue: TStyledButtonClass);
    procedure SetStyleAppearance(const AValue: TStyledButtonAppearance);
    procedure StyleChanged(Sender: TObject);
    procedure ApplyButtonStyle;

    procedure SetImages(const AValue: TCustomImageList);
    procedure SetImageIndex(const AValue: TImageIndex);
    {$IFDEF D10_4+}
    procedure SetImageName(const AValue: TImageName);
    function IsImageNameStored: Boolean;
    {$ENDIF}
    function IsImageIndexStored: Boolean;

    function GetAttributes(const AMode: TStyledButtonMode): TStyledButtonAttributes;
    procedure ImageMarginsChange(Sender: TObject);
    procedure SetImageAlignment(const AValue: TImageAlignment);
    procedure DrawButton;
    procedure DrawBorder;
    procedure DrawImage(const IX, IY: Integer);
    procedure DrawText(const AText: string; var ARect: TRect; AFlags: Cardinal);
    function GetDrawingStyle: TStyledButtonAttributes;
    function UseVCLStyles: Boolean;
    function IsCustomBorderType: Boolean;
    procedure SetBorderType(const AValue: TBtnBorder);
    procedure ImageListChange(Sender: TObject);
    function GetText: TCaption;
    function IsCaptionStored: Boolean;
    procedure SetText(const AValue: TCaption);
    procedure SetButtonStyleDown(const AValue: TStyledButtonAttributes);
    procedure SetButtonStyleFocused(const AValue: TStyledButtonAttributes);
    procedure SetButtonStyleHot(const AValue: TStyledButtonAttributes);
    procedure SetButtonStyleNormal(const AValue: TStyledButtonAttributes);
    procedure SetButtonStyleDisabled(const AValue: TStyledButtonAttributes);
    function IsStyleFocusedStored: Boolean;
    function IsStyleHotStored: Boolean;
    function IsStyleNormalStored: Boolean;
    function IsStyleDisabledStored: Boolean;
    function IsStyleDownStored: Boolean;
  protected
    function GetFocused: Boolean; virtual;
    function GetCanFocus: Boolean; virtual;
    procedure UpdateImage; virtual;
    {$IFDEF D10_4+}
    procedure CheckImageIndexes;
    {$ENDIF}
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure SetName(const AValue: TComponentName); override;
    procedure Loaded; override;
    procedure DoKeyUp;
    procedure Paint; override;
    procedure DoKeyDown(var AKey: Word; AShift: TShiftState);
    {$IFDEF HiDPISupport}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ENDIF}
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMMouseEnter(var Message: TNotifyEvent); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TNotifyEvent); message CM_MOUSELEAVE;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function CalcImageRect(var ATextRect: TRect): TRect;
  public
    procedure AssignStyleTo(ADest: TStyledGraphicButton); virtual;
    procedure AssignTo(ADest: TPersistent); override;
    procedure Click; override;
    procedure SetButtonStyles(
      const AFamily: TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance);

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    constructor CreateStyled(AOwner: TComponent;
      const AFamily: TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance); virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Focused: Boolean read GetFocused;
  published
    property Action;
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
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
    property ParentFont default true;
    property ParentShowHint;
    property ShowHint;
    property Touch;
    property Visible;

    property BorderType: TBtnBorder read FBorderType write SetBorderType stored IsCustomBorderType default btRounded;
    property Caption: TCaption read GetText write SetText stored IsCaptionStored;
    property Cursor default crHandPoint;
    property Height default DEFAULT_BTN_HEIGHT;
    property ImageAlignment: TImageAlignment read FImageAlignment write SetImageAlignment default iaLeft;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex stored IsImageIndexStored default -1;
    {$IFDEF D10_4+}
    property ImageName: TImageName read FImageName write SetImageName stored IsImageNameStored;
    {$ENDIF}
    property ImageMargins: TImageMargins read FImageMargins write SetImageMargins;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property Images: TCustomImageList read FImages write SetImages;
    property Radius: Integer read FRadius write SetRadius stored IsRadius default DEFAULT_RADIUS;
    property StyleFamily: TStyledButtonFamily read FStyleFamily write SetStyleFamily stored IsStoredStyleFamily;
    property StyleClass: TStyledButtonClass read FStyleClass write SetStyleClass stored IsStoredStyleClass;
    property StyleAppearance: TStyledButtonAppearance read FStyleAppearance write SetStyleAppearance stored IsStoredStyleAppearance;
    property Tag: Integer read FTag write FTag;
    property Width default DEFAULT_BTN_WIDTH;

    property ButtonStyleNormal: TStyledButtonAttributes read FButtonStyleNormal write SetButtonStyleNormal stored IsStyleNormalStored;
    property ButtonStyleDown: TStyledButtonAttributes read FButtonStyleDown write SetButtonStyleDown stored IsStyleDownStored;
    property ButtonStyleFocused: TStyledButtonAttributes read FButtonStyleFocused write SetButtonStyleFocused stored IsStyleFocusedStored;
    property ButtonStyleHot: TStyledButtonAttributes read FButtonStyleHot write SetButtonStyleHot stored IsStyleHotStored;
    property ButtonStyleDisabled: TStyledButtonAttributes read FButtonStyleDisabled write SetButtonStyleDisabled stored IsStyleDisabledStored;
  end;

  TStyledButton = class(TStyledGraphicButton)
  private
    FTabStop: Boolean;
    FFocusControl: TButtonFocusControl;
    function GetTabOrder: Integer;
    procedure SetTabStop(const AValue: Boolean);
    function GetTabStop: Boolean;
    procedure SetTabOrder(const AValue: Integer);
    procedure DestroyFocusControl;
    procedure CreateFocusControl(AOwner: TComponent; AParent: TWinControl);
  protected
    function GetFocused: Boolean; override;
    function GetCanFocus: Boolean; override;
    procedure VisibleChanging; override;
    procedure SetName(const AValue: TComponentName); override;
    procedure SetParent(AParent: TWinControl); override;
  public
    procedure SetFocus;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CanFocus: Boolean read GetCanFocus;
  published
    property TabOrder: Integer read GetTabOrder write SetTabOrder;
    property TabStop: Boolean read GetTabStop write SetTabStop default True;
  end;

implementation

uses
  System.Types
  , Vcl.Forms
  , Vcl.ActnList
  , Vcl.StandardButtonStyles
//  , Winapi.GDIPAPI
//  , Winapi.GDIPOBJ
  ;

{ TButtonFocusControl }

constructor TButtonFocusControl.Create(AOwner: TComponent; AStyledButton: TStyledButton);
begin
  inherited Create(AOwner);
  FButton := AStyledButton;
  Self.TabStop := false;
end;

procedure TButtonFocusControl.WMKeyDown(var Message: TWMKeyDown);
var
  Shift: TShiftState;
begin
  if Assigned(FButton) then
  begin
    Shift := KeyDataToShiftState(Message.KeyData);
    FButton.DoKeyDown(Message.CharCode, Shift);
  end;
  inherited;
end;

procedure TButtonFocusControl.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    WM_SETFOCUS, WM_KILLFOCUS:
      begin
        if Assigned(FButton) then
          FButton.Repaint;
      end;
  end;
end;

procedure TButtonFocusControl.WMKeyUp(var Message: TWMKeyDown);
begin
  if Assigned(FButton) then
    FButton.DoKeyUp;
  inherited;
end;

{ TGraphicButton }

procedure TStyledGraphicButton.AssignStyleTo(ADest: TStyledGraphicButton);
begin
  ADest.FBrush.Assign(Self.FBrush);
  ADest.FPen.Assign(Self.FPen);
  ADest.Font.Assign(Self.Font);
  ADest.FRadius := Self.FRadius;
  ADest.FImageMargins.Assign(FImageMargins);
  ADest.FBorderType := Self.FBorderType;
  ADest.FButtonStyleNormal.Assign(Self.FButtonStyleNormal);
  ADest.FButtonStyleDown.Assign(Self.FButtonStyleDown);
  ADest.FButtonStyleFocused.Assign(Self.FButtonStyleFocused);
  ADest.FButtonStyleHot.Assign(Self.FButtonStyleHot);
  ADest.FButtonStyleDisabled.Assign(Self.FButtonStyleDisabled);
  ADest.SetButtonStyles(Self.FStyleFamily,
    Self.FStyleClass, Self.FStyleAppearance);
  if Assigned(FImages) then
  begin
    ADest.FImageAlignment := Self.FImageAlignment;
    ADest.Images := Images;
    ADest.ImageIndex := Self.ImageIndex;
    {$IFDEF D10_4+}
    ADest.ImageName := Self.ImageName;
    {$ENDIF}
  end;
end;

procedure TStyledGraphicButton.AssignTo(ADest: TPersistent);
var
  LDest: TStyledGraphicButton;
begin
  if ADest is TStyledGraphicButton then
  begin
    LDest := TStyledGraphicButton(ADest);
    AssignStyleTo(LDest);
    LDest.Caption := Self.Caption;
    LDest.Hint := Self.Hint;
    LDest.FModalResult := Self.FModalResult;
    LDest.FTag := Self.Tag;
  end;
end;

{$IFDEF HiDPISupport}
procedure TStyledGraphicButton.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  FPen.Width := MulDiv(FPen.Width, M, D);
  FImageMargins.Left := MulDiv(FImageMargins.Left, M, D);
  FImageMargins.Top := MulDiv(FImageMargins.Top, M, D);
  FImageMargins.Right := MulDiv(FImageMargins.Right, M, D);
  FImageMargins.Bottom := MulDiv(FImageMargins.Bottom, M, D);
  inherited;
end;
{$ENDIF}

procedure TStyledGraphicButton.CMEnter(var Message: TCMEnter);
begin
  if not(Enabled) or (csDesigning in ComponentState) then
    Exit;

  FMouseInControl := false;
  Invalidate;
end;

procedure TStyledGraphicButton.CMMouseEnter(var Message: TNotifyEvent);
begin
  if not(Enabled) or (csDesigning in ComponentState) then
    Exit;

  FMouseInControl := true;
  Invalidate;
end;

procedure TStyledGraphicButton.CMMouseLeave(var Message: TNotifyEvent);
begin
  if not(Enabled) or (csDesigning in ComponentState) then
    Exit;

  FMouseInControl := false;
  Invalidate;
end;

procedure TStyledGraphicButton.Click;
begin
  inherited;
end;

procedure TStyledGraphicButton.ImageListChange(Sender: TObject);
begin
  UpdateImage;
  Invalidate;
end;

procedure TStyledGraphicButton.ImageMarginsChange(Sender: TObject);
begin
  Invalidate;
end;

constructor TStyledGraphicButton.CreateStyled(AOwner: TComponent;
  const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance);
begin
  inherited Create(AOwner);
  FButtonStyleNormal := TStyledButtonAttributes.Create(Self);
  FButtonStyleNormal.Name := 'Normal';
  FButtonStyleDown := TStyledButtonAttributes.Create(Self);
  FButtonStyleDown.Name := 'Down';
  FButtonStyleFocused := TStyledButtonAttributes.Create(Self);
  FButtonStyleFocused.Name := 'Focused';
  FButtonStyleHot := TStyledButtonAttributes.Create(Self);
  FButtonStyleHot.Name := 'Hot';
  FButtonStyleDisabled := TStyledButtonAttributes.Create(Self);
  FButtonStyleDisabled.Name := 'Disabled';

  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption, csDoubleClicks];
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FImageMargins := TImageMargins.Create;
  FImageMargins.OnChange := ImageMarginsChange;
  FImageAlignment := iaLeft;
  FBorderType := btrounded;
  Cursor := crHandPoint;
  ControlStyle := ControlStyle + [csReplicatable];
  ParentFont := true;
  Width := DEFAULT_BTN_WIDTH;
  Height := DEFAULT_BTN_HEIGHT;
  FRadius := DEFAULT_RADIUS;
  // Pen
  FPen := TPen.Create;
  FPen.Style := psClear;
  FPen.OnChange := StyleChanged;
  // Brush
  FBrush := TBrush.Create;
  FBrush.OnChange := StyleChanged;
  FMouseInControl := false;
  FImageIndex := -1;
  {$IFDEF D10_4+}
  FImageName := '';
  {$ENDIF}
  FStyleFamily := AFamily;
  FStyleClass := AClass;
  FStyleAppearance := AAppearance;
  ApplyButtonStyle;
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

procedure TStyledGraphicButton.ApplyButtonStyle;
begin
  StyleFamilyCheckAttributes(FStyleFamily,
    FStyleClass, FStyleAppearance);
  StyleFamilyUpdateAttributes(
    FStyleFamily,
    FStyleClass,
    FstyleAppearance,
    FButtonStyleNormal,
    FButtonStyleDown,
    FButtonStyleFocused,
    FButtonStyleHot,
    FButtonStyleDisabled);

  invalidate;
end;

destructor TStyledGraphicButton.Destroy;
begin
  Images := nil;
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FPen);
  FreeAndNil(FBrush);
  FreeAndNil(FImageMargins);
  FreeAndNil(FButtonStyleNormal);
  FreeAndNil(FButtonStyleDown);
  FreeAndNil(FButtonStyleFocused);
  FreeAndNil(FButtonStyleHot);
  FreeAndNil(FButtonStyleDisabled);

  inherited Destroy;
end;

procedure TStyledGraphicButton.DoKeyDown(var AKey: Word; AShift: TShiftState);
begin
  if ((AKey = VK_RETURN) or (AKey = VK_SPACE)) then
  begin
    FState := bsDown;
    Invalidate;

    Self.Click;
  end;
end;

procedure TStyledGraphicButton.DoKeyUp;
begin
  FState := bsUp;

  Invalidate;
end;

function TStyledGraphicButton.GetPictureHeight: Integer;
begin
  if IconAssigned then
    Result := Images.Height
  else
    Result := 0;
end;

function TStyledGraphicButton.GetPictureWidth: Integer;
begin
  if IconAssigned then
    Result := Images.Width
  else
    Result := 0;
end;

function TStyledGraphicButton.GetText: TCaption;
begin
  Result := inherited Caption;
end;

function TStyledGraphicButton.IconAssigned: Boolean;
begin
  Result := Assigned(Images) and ((FImageIndex <> -1)
    {$IFDEF D10_4+}or (FImageName <> ''){$ENDIF});
end;

function TStyledGraphicButton.IsCaptionStored: Boolean;
begin
  Result := (ActionLink = nil) or
    not TGraphicButtonActionLink(ActionLink).IsCaptionLinked;
end;

function TStyledGraphicButton.IsImageIndexStored: Boolean;
begin
  Result := (ActionLink = nil) or
    not TGraphicButtonActionLink(ActionLink).IsImageIndexLinked;
end;

function TStyledGraphicButton.IsCustomBorderType: Boolean;
begin
  Result := FBorderType <> GetDrawingStyle.BorderType;
end;

function TStyledGraphicButton.IsDefault: Boolean;
begin
  //IsDefault := (Control is TCustomButton) and TCustomButton(Control).FActive;
  Result := False;
end;

function TStyledGraphicButton.IsPressed: Boolean;
begin
  Result := FState = bsDown;
end;

function TStyledGraphicButton.IsRadius: Boolean;
begin
  Result := Radius <> DEFAULT_RADIUS;
end;

function TStyledGraphicButton.IsStoredStyleFamily: Boolean;
begin
  Result := FStyleFamily <> DEFAULT_CLASSIC_FAMILY;
end;

function TStyledGraphicButton.IsStoredStyleClass: Boolean;
begin
  Result := FStyleClass <> DEFAULT_WINDOWS_CLASS;
end;

function TStyledGraphicButton.IsStoredStyleAppearance: Boolean;
begin
  Result := FStyleAppearance <> DEFAULT_APPEARANCE;
end;

function TStyledGraphicButton.IsStyleDisabledStored: Boolean;
begin
  Result := FButtonStyleDisabled.IsChanged;
end;

function TStyledGraphicButton.IsStyleDownStored: Boolean;
begin
  Result := FButtonStyleDown.IsChanged;
end;

function TStyledGraphicButton.IsStyleFocusedStored: Boolean;
begin
  Result := FButtonStyleFocused.IsChanged;
end;

function TStyledGraphicButton.IsStyleHotStored: Boolean;
begin
  Result := FButtonStyleHot.IsChanged;
end;

function TStyledGraphicButton.IsStyleNormalStored: Boolean;
begin
  Result := FButtonStyleNormal.IsChanged;
end;

function TStyledGraphicButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TGraphicButtonActionLink;
end;

function TStyledGraphicButton.GetAttributes(const AMode: TStyledButtonMode): TStyledButtonAttributes;
begin
  case Amode of
    bsmDown: Result := FButtonStyleDown;
    bsmFocused: Result := FButtonStyleFocused;
    bsmHot: Result := FButtonStyleHot;
    bsmDisabled: Result := FButtonStyleDisabled;
  else
    Result := FButtonStyleNormal;
  end;
end;

function TStyledGraphicButton.GetCanFocus: Boolean;
begin
  Result := False;
end;

procedure TStyledGraphicButton.DrawText(const AText: string; var ARect: TRect; AFlags: Cardinal);
var
  Details:  TThemedElementDetails;
  ThemeTextColor: TColor;
  TextFormat: TTextFormatFlags;
  LStyle: TCustomStyleServices;
  R: TRect;

  function CanvasDrawText(ACanvas: TCanvas; const AText: string; var Bounds: TRect; Flag: Cardinal): Integer;
  begin
    SetBkMode(ACanvas.Handle, TRANSPARENT);
    Result := Winapi.Windows.DrawText(ACanvas.Handle, PChar(AText), Length(AText), Bounds, Flag)
  end;

begin
  TextFormat := TTextFormatFlags(AFlags);
  if UseVCLStyles then
  begin
    LStyle := StyleServices;
    if LStyle.GetElementColor(Details, ecTextColor, ThemeTextColor) then
    begin
      if not Enabled or (seFont in StyleElements) then
        Canvas.Font.Color := ThemeTextColor;
      LStyle.DrawText(Canvas.Handle, Details, AText, ARect, TextFormat, Canvas.Font.Color);
    end
    else
    begin
      Canvas.Refresh;
      LStyle.DrawText(Canvas.Handle, Details, AText, ARect, TextFormat);
    end;
  end
  else
  begin
    //Drawing Caption
    R := ARect;
    CanvasDrawText(Canvas, AText, R, AFlags or DT_CALCRECT);
    OffsetRect(R, (ARect.Width - R.Width) div 2, (ARect.Height - R.Height) div 2);
    CanvasDrawText(Canvas, AText, R, AFlags);
  end;
end;

function TStyledGraphicButton.UseVCLStyles: Boolean;
begin
  Result := False;
end;

(*
function CreateRoundRectangle(rectangle: TGPRect;
  radius: integer): TGPGraphicsPath;
var
  path : TGPGraphicsPath;
  l, t, w, h, d : integer;
begin
  path := TGPGraphicsPath.Create;
  l := rectangle.X;
  t := rectangle.y;
  w := rectangle.Width;
  h := rectangle.Height;
  d := radius div 2; // divide by 2

  // the lines beween the arcs are automatically added by the path
  path.AddArc(l, t, d, d, 180, 90); // topleft
  path.AddArc(l + w - d, t, d, d, 270, 90); // topright
  path.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
  path.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
  path.CloseFigure();
  result := path;
end;
*)

procedure TStyledGraphicButton.DrawBorder;
var
  Details:  TThemedElementDetails;
  LStyle: TCustomStyleServices;
  DrawRect: TRect;
(*
  GPGraphics: TGPGraphics;
  GPPen: TGPPen;
  GPRect: TGPRect;

  function ConvertColor(Color: TColor; Alpha: Byte): Cardinal;
  begin
    with TColors(Color) do
      Result := Winapi.GDIPAPI.MakeColor(Alpha, R, G, B);
  end;
*)
begin
  DrawRect := ClientRect;

  if UseVCLStyles then
  begin
    LStyle := StyleServices;
    if IsPressed then
    begin
      Details := LStyle.GetElementDetails(tbPushButtonPressed);
    end
    else if FMouseInControl then
    begin
      Details := LStyle.GetElementDetails(tbPushButtonHot);
    end
    else if Focused or IsDefault then
    begin
      Details := LStyle.GetElementDetails(tbPushButtonDefaulted);
    end
    else if Enabled then
      Details := LStyle.GetElementDetails(tbPushButtonNormal);

    LStyle.DrawElement(Canvas.Handle, Details, DrawRect);
  end
  else
  begin
    //Reduce Canvas
    if Canvas.Pen.Style = psSolid then
      InflateRect(DrawRect, -Canvas.Pen.Width div 2, -Canvas.Pen.Width div 2)
    else
      InflateRect(DrawRect, Canvas.Pen.Width div 2, Canvas.Pen.Width div 2);

    //Draw Rectancle or RoundRect
    case FBorderType of
      btedgy:
      begin
        Canvas.Rectangle(DrawRect.Left, DrawRect.Top,
          DrawRect.Left + DrawRect.Width,
          DrawRect.Top + DrawRect.Height);
      end;
      btRounded:
      begin
        Canvas.RoundRect(DrawRect.Left, DrawRect.Top,
          DrawRect.Left + DrawRect.Width, DrawRect.Top + DrawRect.Height,
          FRadius, FRadius);
        (*
        GPRect.X := DrawRect.Left;
        GPRect.Y := DrawRect.Top;
        GPRect.Width := DrawRect.Width;
        GPRect.Height := DrawRect.Height;
        GPGraphics := TGPGraphics.Create(Canvas.Handle);
        GPPen := TGPPen.Create(ConvertColor(Canvas.Pen.Color, 255));
        try
          GPGraphics.DrawPath(GPPen, CreateRoundRectangle(GPRect, Radius));
        finally
          GPPen.Free;
          GPGraphics.Free;
        end;
        *)
      end;
    end;
  end;
end;

procedure TStyledGraphicButton.DrawImage(
  const IX, IY: Integer);
begin
  if IconAssigned then
    Images.Draw(Canvas, IX, IY, FImageIndex, Enabled);
end;

function TStyledGraphicButton.CalcImageRect(var ATextRect: TRect): TRect;
var
  IW, IH, IX, IY: Integer;
begin
  //Calc Image Rect
  IH := GetPictureHeight;
  IW := GetPictureWidth;
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

procedure TStyledGraphicButton.SetButtonStyles(
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
procedure TStyledGraphicButton.CheckImageIndexes;
begin
  if (Images = nil) or not Images.IsImageNameAvailable then
    Exit;
  Images.CheckIndexAndName(FImageIndex, FImageName);
end;
{$ENDIF}

procedure TStyledGraphicButton.DrawButton;
var
  LTextFlags: Cardinal;
  LTextRect, LImageRect: TRect;
begin
  GetDrawingStyle;
  LTextFlags := 0;
  DrawBorder;

  LTextRect := ClientRect;
  LImageRect := CalcImageRect(LTextRect);
  DrawImage(LImageRect.Left, LImageRect.Top);

  if LTextRect.IsEmpty then
    LTextRect := ClientRect;
  DrawText(Caption, LTextRect, DrawTextBiDiModeFlags(LTextFlags));
end;

function TStyledGraphicButton.GetDrawingStyle: TStyledButtonAttributes;
begin
  //Getting drawing styles
  if not Enabled then
    Result := GetAttributes(bsmDisabled)
  else if FState = bsDown then
    Result := GetAttributes(bsmDown)
  else if FMouseInControl and not Focused then
    Result := GetAttributes(bsmHot)
  else if Focused then
    Result := GetAttributes(bsmFocused)
  else
    Result := GetAttributes(bsmNormal);
  Canvas.Pen := FPen;
  Canvas.Pen.Style := Result.BorderStyle;
  if Canvas.Pen.Style <> psClear then
    Canvas.Pen.Width := Round(Result.BorderWidth)
  else
    Canvas.Pen.Width := 0;
  Canvas.Pen.Color := Result.BorderColor;
  Canvas.Brush.Style := Result.BrushStyle;
  if Canvas.Brush.Style <> bsClear then
    Canvas.Brush.Color := Result.ButtonColor;
  Canvas.Font := Font;
  Canvas.Font.Name := Result.FontName;
  Canvas.Font.Color := Result.FontColor;
  Canvas.Font.Style := Result.FontStyle;
  FBorderType := Result.BorderType;
end;

function TStyledGraphicButton.GetFocused: Boolean;
begin
  Result := False;
end;

procedure TStyledGraphicButton.Paint;
begin
  DrawButton;
end;

procedure TStyledGraphicButton.SetBorderType(const AValue: TBtnBorder);
begin
  if FBorderType <> AValue then
  begin
    FBorderType := AValue;
    Invalidate;
  end;
end;

procedure TStyledGraphicButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  Repaint;
end;

procedure TStyledGraphicButton.SetImageAlignment(const AValue: TImageAlignment);
begin
  if AValue <> FImageAlignment then
  begin
    FImageAlignment := AValue;
    Invalidate;
  end;
end;

procedure TStyledGraphicButton.SetImageIndex(const AValue: TImageIndex);
begin
  if AValue <> FImageIndex then
  begin
    FImageIndex := AValue;
    {$IFDEF D10_4+}
    if (FImages <> nil) and FImages.IsImageNameAvailable then
       FImageName := FImages.GetNameByIndex(FImageIndex);
    {$ENDIF}
    UpdateImage;
    Invalidate;
  end;
end;

{$IFDEF D10_4+}
function TStyledGraphicButton.IsImageNameStored: Boolean;
begin
  Result := (ActionLink = nil) or
    not TGraphicButtonActionLink(ActionLink).IsImageNameLinked;
end;

procedure TStyledGraphicButton.SetImageName(const AValue: TImageName);
begin
  if AValue <> FImageName then
  begin
    FImageName := AValue;
    if (FImages <> nil) and FImages.IsImageNameAvailable then
      FImageIndex := FImages.GetIndexByName(FImageName);
    UpdateImage;
    Invalidate;
  end;
end;
{$ENDIF}

procedure TStyledGraphicButton.UpdateImage;
begin
{$IFDEF D10_4+}
  if (FImages <> nil) and FImages.IsImageNameAvailable then
  begin
    if (FImageName <> '') and (FImageIndex = -1) then
      FImageIndex := FImages.GetIndexByName(FImageName)
    else if (FImageName = '') and (FImageIndex <> -1) then
      FImageName := FImages.GetNameByIndex(FImageIndex);
    CheckImageIndexes;
  end;
{$ENDIF}
end;

procedure TStyledGraphicButton.SetImages(const AValue: TCustomImageList);
begin
  if AValue <> FImages then
  begin
    if FImages <> nil then
    begin
      FImages.RemoveFreeNotification(Self);
      FImages.UnRegisterChanges(FImageChangeLink);
    end;
    FImages := AValue;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(Self);
    end;
    UpdateImage;
    Invalidate;
  end;
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

procedure TStyledGraphicButton.SetButtonStyleNormal(const AValue: TStyledButtonAttributes);
begin
  if not SameStyledButtonStyle(FButtonStyleNormal, AValue) then
  begin
    FButtonStyleNormal := AValue;
  end;
end;

procedure TStyledGraphicButton.SetButtonStyleDisabled(
  const AValue: TStyledButtonAttributes);
begin
  if not SameStyledButtonStyle(FButtonStyleDisabled, AValue) then
  begin
    FButtonStyleDisabled := AValue;
  end;
end;

procedure TStyledGraphicButton.SetButtonStyleDown(const AValue: TStyledButtonAttributes);
begin
  if not SameStyledButtonStyle(FButtonStyleDown, AValue) then
  begin
    FButtonStyleDown := AValue;
  end;
end;

procedure TStyledGraphicButton.SetButtonStyleFocused(const AValue: TStyledButtonAttributes);
begin
  if not SameStyledButtonStyle(FButtonStyleFocused, AValue) then
  begin
    FButtonStyleFocused := AValue;
  end;
end;

procedure TStyledGraphicButton.SetButtonStyleHot(const AValue: TStyledButtonAttributes);
begin
  if not SameStyledButtonStyle(FButtonStyleHot, AValue) then
  begin
    FButtonStyleHot := AValue;
  end;
end;

procedure TStyledGraphicButton.SetImageMargins(const AValue: TImageMargins);
begin
  FImageMargins.Assign(AValue);
end;

procedure TStyledGraphicButton.SetRadius(const AValue: Integer);
begin
  if FRadius <> AValue then
  begin
    FRadius := AValue;
    Invalidate;
  end;
end;

procedure TStyledGraphicButton.SetStyleAppearance(
  const AValue: TStyledButtonAppearance);
var
  LValue: TStyledButtonAppearance;
begin
  LValue := AValue;
  if LValue = '' then
    LValue := DEFAULT_APPEARANCE;
  if LValue <> Self.FStyleAppearance then
  begin
    Self.FStyleAppearance := LValue;
    ApplyButtonStyle;
  end;
end;

procedure TStyledGraphicButton.SetStyleClass(
  const AValue: TStyledButtonClass);
var
  LValue: TStyledButtonClass;
begin
  LValue := AValue;
  if LValue = '' then
    LValue := DEFAULT_WINDOWS_CLASS;
  if LValue <> Self.FStyleClass then
  begin
    Self.FStyleClass := LValue;
    ApplyButtonStyle;
  end;
end;

procedure TStyledGraphicButton.SetStyleFamily(
  const AValue: TStyledButtonFamily);
var
  LValue: TStyledButtonFamily;
begin
  LValue := AValue;
  if LValue = '' then
    LValue := DEFAULT_CLASSIC_FAMILY;
  if LValue <> Self.FStyleFamily then
  begin
    FStyleFamily := LValue;
    ApplyButtonStyle;
  end;
end;

procedure TStyledGraphicButton.SetText(const AValue: TCaption);
begin
  inherited Caption := AValue;
  Invalidate;
end;

procedure TStyledGraphicButton.StyleChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TStyledGraphicButton.Loaded;
begin
  inherited;
  SetImageIndex(ImageIndex);
  ApplyButtonStyle;
end;

procedure TStyledGraphicButton.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if AOperation = opRemove then
  begin
    if AComponent = Images then
      Images := nil;
  end;
end;

{ TGraphicButtonActionLink }

procedure TGraphicButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TStyledGraphicButton;
end;

function TGraphicButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked;
   (*and (FClient.Checked = TCustomAction(Action).Checked);*)
end;

function TGraphicButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (TStyledGraphicButton(FClient).ImageIndex = TStyledGraphicButton(Action).ImageIndex);
end;

{$IFDEF D10_4+}
function TGraphicButtonActionLink.IsImageNameLinked: Boolean;
begin
  Result := inherited IsImageNameLinked and
    (TStyledGraphicButton(FClient).ImageName =
      TCustomAction(Action).ImageName);
end;
{$ENDIF}

procedure TGraphicButtonActionLink.SetChecked(Value: Boolean);
begin
  inherited;
  ;
end;

procedure TGraphicButtonActionLink.SetImageIndex(Value: Integer);
begin
  inherited;
  if IsImageIndexLinked then
    TStyledGraphicButton(FClient).ImageIndex := Value;
end;

{ TStyledButton }

procedure TStyledButton.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TStyledButton then
    TStyledButton(Dest).TabStop := Self.TabStop;
end;

function TStyledButton.GetCanFocus: Boolean;
begin
  if Assigned(FFocusControl) then
    Result := FFocusControl.CanFocus
  else
    Result := False;
end;

procedure TStyledButton.Click;
begin
  inherited;
  SetFocus;
end;

constructor TStyledButton.Create(AOwner: TComponent);
begin
  inherited;
  FFocusControl := nil;
  CreateFocusControl(nil, TWinControl(AOwner));
  TabStop := True;
end;

procedure TStyledButton.CreateFocusControl(AOwner: TComponent;
  AParent: TWinControl);
begin
  if not Assigned(FFocusControl) then
  begin
    FFocusControl := TButtonFocusControl.Create(AOwner, Self);
    try
      FFocusControl.TabStop := true;
      FFocusControl.SetBounds(0, 0, 0, 0);
    except
      raise;
    end;
  end;
end;

destructor TStyledButton.Destroy;
begin
  DestroyFocusControl;
  inherited;
end;

procedure TStyledButton.DestroyFocusControl;
begin
  if Assigned(FFocusControl) then
  begin
    if Assigned(FFocusControl.Parent) then
      FreeAndNil(FFocusControl);
  end;
end;

procedure TStyledButton.SetFocus;
begin
  if Assigned(FFocusControl) then
    if FFocusControl.CanFocus then
    FFocusControl.SetFocus;
end;

procedure TStyledButton.SetName(const AValue: TComponentName);
begin
  if Assigned(FFocusControl) then
    FFocusControl.Name := AValue;
  inherited;
end;

procedure TStyledButton.SetParent(AParent: TWinControl);
begin
  inherited;
  if Assigned(Self.Parent) then
  begin
    FFocusControl.Parent := Self.Parent;
    FFocusControl.Show;
  end;
end;

procedure TStyledButton.SetTabOrder(const AValue: Integer);
begin
  if Assigned(FFocusControl) then
    FFocusControl.TabOrder := AValue;
end;

procedure TStyledButton.SetTabStop(const AValue: Boolean);
begin
  FTabStop := AValue;
  if Assigned(FFocusControl) then
    FFocusControl.TabStop := AValue;
end;

procedure TStyledButton.VisibleChanging;
begin
  inherited;
  if Assigned(FFocusControl) then
  begin
    FFocusControl.Visible := not Self.Visible;
    FFocusControl.TabStop := FTabStop;
  end;
end;

function TStyledButton.GetFocused: Boolean;
begin
  if Assigned(FFocusControl) then
    Result := FFocusControl.Focused
  else
    Result := false;
end;

function TStyledButton.GetTabOrder: Integer;
begin
  if Assigned(FFocusControl) then
    Result := FFocusControl.TabOrder
  else
    Result := -1;
end;

function TStyledButton.GetTabStop: Boolean;
begin
  if Assigned(FFocusControl) then
    Result := FFocusControl.TabStop
  else
    Result := false;
end;

procedure TStyledButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SetFocus;
  inherited;
end;

end.
