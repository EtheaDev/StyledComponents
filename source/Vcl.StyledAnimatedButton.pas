{******************************************************************************}
{                                                                              }
{  TStyledAnimatedButton: a StyledButton with "animated icon"                  }
{  using a Skia TSkAnimatedImage component                                     }
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
unit Vcl.StyledAnimatedButton;

interface

{$INCLUDE StyledComponents.inc}

uses
  Winapi.Windows
  , Winapi.Messages
  , System.SysUtils
  , System.Types
  , System.Classes
  , System.UITypes
  , Vcl.Controls
  , Vcl.ImgList
  , Vcl.Graphics
  , Vcl.StyledButton
  , Vcl.ButtonStylesAttributes
  , Vcl.Skia
  , Vcl.SkAnimatedImageHelper
  ;

const
  DEFAULT_ANIM_SIZE = 32;
  DEFAULT_ANIM_MARGIN = 10;

type
  TAutoAnimationType = (AnimateOnMouseOver, AnimateOnClick, AnimateAlways, AnimateOnFocus);
  TAutoAnimationTypes = set of TAutoAnimationType;
  TAnimateOnType = (AnimateOnButton, AnimateOnImage);

  { TStyledAnimatedButtonRender }
  TStyledAnimatedButtonRender = class(TStyledButtonRender)
  private
    FAnimationHeight: Integer;
    FAnimationWidth: Integer;
    FAutoSizeAnimation: Boolean;
    FAutoSizeAnimationMargin: Integer;
    procedure SetAutoSizeAnimationMargin(const AValue: Integer);
    procedure SetAnimationHeight(const AValue: Integer);
    procedure SetAnimationWidth(const AValue: Integer);
    procedure SetAutoSizeAnimation(const AValue: Boolean);
  public
    {$IFDEF HiDPISupport}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ENDIF}
    function GetImageSize(out AWidth: Integer; out AHeight: Integer; out AImageList: TCustomImageList; out AImageIndex: Integer): Boolean; override;
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
      const AUseCustomDrawType: Boolean); override;
    property AnimationHeight: Integer read FAnimationHeight write SetAnimationHeight;
    property AnimationWidth: Integer read FAnimationWidth write SetAnimationWidth;
    property AutoSizeAnimation: Boolean read FAutoSizeAnimation write SetAutoSizeAnimation;
    property AutoSizeAnimationMargin: Integer read FAutoSizeAnimationMargin write SetAutoSizeAnimationMargin;
  end;

  { TStyledAnimatedButton }
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TStyledAnimatedButton = class(TStyledButton)
  strict private
    FSkAnimatedImage: TSkAnimatedImage;
    procedure AnimatedImageClick(Sender: TObject);
    procedure AnimatedImageDblClick(Sender: TObject);
    procedure AnimatedImageMouseEnter(Sender: TObject);
    procedure AnimatedImageMouseLeave(Sender: TObject);
  private
    FAutoAnimationTypes: TAutoAnimationTypes;
    FAnimateOnType: TAnimateOnType;
    function GetAnimatedImage: TSkAnimatedImage;
    function GetSource: TSkAnimatedImage.TSource;
    procedure SetSource(const AValue: TSkAnimatedImage.TSource);
    procedure SetAnimationHeight(const AValue: Integer);
    procedure SetAnimationWidth(const AValue: Integer);
    procedure SetAutoSizeAnimation(const AValue: Boolean);
    procedure SetAutoSizeAnimationMargin(const AValue: Integer);
    procedure SetAutoAnimationTypes(const AValue: TAutoAnimationTypes);
    procedure SetAnimationInverse(const AValue: Boolean);
    procedure SetAnimationLoop(const AValue: Boolean);
    procedure SetAnimateOnType(const AValue: TAnimateOnType);

    //Windows Messages
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;

    procedure ReadData(AStream: TStream);
    procedure WriteData(AStream: TStream);
    function GetAnimationInverse: Boolean;
    function GetAnimationLoop: Boolean;
    function GetAnimationHeight: Integer;
    function GetAnimationWidth: Integer;
    function GetAutoSizeAnimation: Boolean;
    function GetAutoSizeAnimationMargin: Integer;
    function GetRender: TStyledAnimatedButtonRender;
  strict protected
    procedure DefineProperties(AFiler: TFiler); override;
  protected
    procedure Loaded; override;
    procedure SetCursor(const AValue: TCursor); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    function GetRenderClass: TStyledButtonRenderClass; override;
  public
    constructor CreateStyled(AOwner: TComponent;
      const AFamily: TStyledButtonFamily;
      const AClass: TStyledButtonClass;
      const AAppearance: TStyledButtonAppearance); override;
    destructor Destroy; override;
    procedure LoadAnimationFromFile(const AFileName: TFileName);
    procedure LoadAnimationFromStream(const AStream: TStream);
    procedure LoadAnimationFromResource(const AResourceName: string);
    procedure Click; override;
    //Animation procedures and functions
    function CanPlayAnimation: boolean;
    procedure StartAnimation(ALoop: Boolean = False;
      AInverse: Boolean = False; AProgress: Double = 0);
    procedure PauseAnimation;
    procedure StopAnimation;
    procedure ResumeAnimation;
    function AnimationIsRunning: Boolean;
    procedure CreateAnimation;
    property AnimatedImage: TSkAnimatedImage read GetAnimatedImage;
    property Render: TStyledAnimatedButtonRender read GetRender;
  published
    property AutoAnimationTypes: TAutoAnimationTypes read FAutoAnimationTypes write SetAutoAnimationTypes default [AnimateOnMouseOver];
    property AnimateOnType: TAnimateOnType read FAnimateOnType write SetAnimateOnType default AnimateOnButton;
    property AutoSizeAnimation: Boolean read GetAutoSizeAnimation write SetAutoSizeAnimation default True;
    property AutoSizeAnimationMargin: Integer read GetAutoSizeAnimationMargin write SetAutoSizeAnimationMargin default DEFAULT_ANIM_MARGIN;
    property AnimationSource: TSkAnimatedImage.TSource read GetSource write SetSource;
    property AnimationWidth: Integer read GetAnimationWidth write SetAnimationWidth default DEFAULT_ANIM_SIZE;
    property AnimationHeight: Integer read GetAnimationHeight write SetAnimationHeight  default DEFAULT_ANIM_SIZE;
    property AnimationLoop: Boolean read GetAnimationLoop write SetAnimationLoop default False;
    property AnimationInverse: Boolean read GetAnimationInverse write SetAnimationInverse default False;
  end;

implementation

uses
  System.Skia
  , System.math
  {$IFDEF D10_3+}
  , Vcl.VirtualImageList
  {$ENDIF}
  ;

{ TStyledAnimatedButtonRender }

procedure TStyledAnimatedButtonRender.SetAutoSizeAnimationMargin(
  const AValue: Integer);
begin
  if FAutoSizeAnimationMargin <> AValue then
  begin
    FAutoSizeAnimationMargin := AValue;
    Invalidate;
  end;
end;

constructor TStyledAnimatedButtonRender.CreateStyled(AOwner: TControl;
  const AOnClick: TNotifyEvent; const AControlFont: TControlFont;
  const AGetCaption: TGetCaption; const ASetCaption: TSetCaption;
  const AGetParentFont: TGetParentFont; const ASetParentFont: TSetParentFont;
  const AFamily: TStyledButtonFamily; const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance;
  const ADrawType: TStyledButtonDrawType;
  const ACursor: TCursor; const AUseCustomDrawType: Boolean);
begin
  inherited;
  AnimationHeight := DEFAULT_ANIM_SIZE;
  AnimationWidth := DEFAULT_ANIM_SIZE;
  AutoSizeAnimationMargin := DEFAULT_ANIM_MARGIN;
  AutoSizeAnimation := True;
end;

function TStyledAnimatedButtonRender.GetImageSize(out AWidth, AHeight: Integer;
  out AImageList: TCustomImageList; out AImageIndex: Integer): Boolean;
var
  LStyledAnimatedButton: TStyledAnimatedButton;
  LSize: Integer;
begin
  Result := inherited GetImageSize(AWidth, AHeight, AImageList, AImageIndex);
  if not Result then
  begin
    if OwnerControl is TStyledAnimatedButton then
    begin
      LStyledAnimatedButton := TStyledAnimatedButton(OwnerControl);
      if LStyledAnimatedButton.AutoSizeAnimation then
      begin
        LSize := Min(OwnerControl.Width, OwnerControl.Height);
        AWidth := LSize - (FAutoSizeAnimationMargin * 2);
        AHeight := AWidth;
      end
      else
      begin
        AWidth := FAnimationWidth;
        AHeight := FAnimationHeight;
      end;
    end;
  end;
end;

procedure TStyledAnimatedButtonRender.SetAnimationHeight(const AValue: Integer);
begin
  if FAnimationHeight <> AValue then
  begin
    FAnimationHeight := AValue;
    FAutoSizeAnimation := False;
    Invalidate;
  end;
end;

procedure TStyledAnimatedButtonRender.SetAnimationWidth(const AValue: Integer);
begin
  if FAnimationWidth <> AValue then
  begin
    FAnimationWidth := AValue;
    FAutoSizeAnimation := False;
    Invalidate;
  end;
end;

procedure TStyledAnimatedButtonRender.SetAutoSizeAnimation(
  const AValue: Boolean);
begin
  if FAutoSizeAnimation <> AValue then
  begin
    FAutoSizeAnimation := AValue;
    Invalidate;
  end;
end;

{$IFDEF HiDPISupport}
procedure TStyledAnimatedButtonRender.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  if isDpiChange then
  begin
    FAnimationHeight := MulDiv(FAnimationHeight, M, D);
    FAnimationWidth := MulDiv(FAnimationWidth, M, D);
    FAutoSizeAnimationMargin := MulDiv(FAutoSizeAnimationMargin, M, D);
  end;
end;
{$ENDIF}

{ TStyledAnimatedButton }

procedure TStyledAnimatedButton.CreateAnimation;
begin
  FSkAnimatedImage := TSkAnimatedImage.Create(Self);
  //By default the animation is stopped on Progress 1 (last frame)
  FSkAnimatedImage.SetProgress(1);
  if not (AnimateAlways in AutoAnimationTypes) then
    FSkAnimatedImage.Animation.Stop;
  FSkAnimatedImage.SetLoop(False);
  FSkAnimatedImage.Parent := Self;
  FSkAnimatedImage.OnClick := AnimatedImageClick;
  FSkAnimatedImage.OnDblClick := AnimatedImageDblClick;
  FSkAnimatedImage.OnMouseEnter := AnimatedImageMouseEnter;
  FSkAnimatedImage.OnMouseLeave := AnimatedImageMouseLeave;
  //FSkAnimatedImage.Visible := False;
  FSkAnimatedImage.Cursor := Cursor;
end;

constructor TStyledAnimatedButton.CreateStyled(
  AOwner: TComponent;
  const AFamily: TStyledButtonFamily;
  const AClass: TStyledButtonClass;
  const AAppearance: TStyledButtonAppearance);
begin
  inherited CreateStyled(AOwner, AFamily, AClass, AAppearance);
  FAutoAnimationTypes := [AnimateOnMouseOver];
end;

procedure TStyledAnimatedButton.ReadData(AStream: TStream);
begin
  if (AStream.Size = 0) and Assigned(FSkAnimatedImage) then
    FSkAnimatedImage.Source.Data := nil
  else
    AnimatedImage.LoadFromStream(AStream);
end;

procedure TStyledAnimatedButton.WriteData(AStream: TStream);
begin
  if Assigned(FSkAnimatedImage) and (FSkAnimatedImage.Source.Data <> nil) then
    AStream.WriteBuffer(FSkAnimatedImage.Source.Data,
      Length(FSkAnimatedImage.Source.Data));
end;

procedure TStyledAnimatedButton.DefineProperties(AFiler: TFiler);

  function DoWrite: Boolean;
  begin
    if AFiler.Ancestor <> nil then
      Result := (not (AFiler.Ancestor is TStyledAnimatedButton)) or
        not TStyledAnimatedButton(AFiler.Ancestor).AnimationSource.Equals(Self.AnimationSource)
    else
      Result := AnimatedImage.Source.Data <> nil;
  end;

begin
  inherited;
  AFiler.DefineBinaryProperty('AnimationData', ReadData, WriteData, DoWrite);
end;

destructor TStyledAnimatedButton.Destroy;
begin
  inherited;
end;

function TStyledAnimatedButton.GetAnimatedImage: TSkAnimatedImage;
begin
  if not Assigned(FSkAnimatedImage) then
    CreateAnimation;
  Result := FSkAnimatedImage;
end;

function TStyledAnimatedButton.GetAnimationHeight: Integer;
begin
  Result := Render.AnimationHeight;
end;

function TStyledAnimatedButton.GetAnimationInverse: Boolean;
begin
  Result := AnimatedImage.GetInverse;
end;

function TStyledAnimatedButton.GetAnimationLoop: Boolean;
begin
  Result := AnimatedImage.GetLoop;
end;

function TStyledAnimatedButton.GetAnimationWidth: Integer;
begin
  Result := Render.AnimationWidth;
end;

function TStyledAnimatedButton.GetAutoSizeAnimation: Boolean;
begin
  Result := Render.AutoSizeAnimation;
end;

function TStyledAnimatedButton.GetAutoSizeAnimationMargin: Integer;
begin
  Result := Render.AutoSizeAnimationMargin;
end;

function TStyledAnimatedButton.GetRender: TStyledAnimatedButtonRender;
begin
  Result := inherited Render as TStyledAnimatedButtonRender;
end;

function TStyledAnimatedButton.GetRenderClass: TStyledButtonRenderClass;
begin
  Result := TStyledAnimatedButtonRender;
end;

function TStyledAnimatedButton.GetSource: TSkAnimatedImage.TSource;
begin
  Result := AnimatedImage.Source;
end;

procedure TStyledAnimatedButton.LoadAnimationFromFile(const AFileName: TFileName);
begin
  AnimatedImage.LoadFromfile(AFileName);
end;

procedure TStyledAnimatedButton.LoadAnimationFromStream(const AStream: TStream);
begin
  AnimatedImage.LoadFromStream(AStream);
end;

procedure TStyledAnimatedButton.LoadAnimationFromResource(
  const AResourceName: string);
var
  LStream: TResourceStream;
begin
  LStream := TResourceStream.Create(HInstance, AResourceName,
    RT_RCDATA);
  try
    AnimatedImage.LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;


procedure TStyledAnimatedButton.Loaded;
begin
  inherited;
  if csDesigning in ComponentState then
    AnimatedImage.StartAnimation;
end;

procedure TStyledAnimatedButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FSkAnimatedImage) then
    StopAnimation;
  inherited;
end;

function TStyledAnimatedButton.CanPlayAnimation: boolean;
begin
  if Assigned(AnimatedImage) then
    Result := AnimatedImage.CanPlayAnimation
  else
    Result := False;
end;

procedure TStyledAnimatedButton.Click;
begin
  if AnimateOnClick in FAutoAnimationTypes then
    AnimatedImage.StartAnimation;
  inherited;
end;

procedure TStyledAnimatedButton.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  inherited;
  if not FDoubleBuffered or
     (TMessage(Message).wParam = WPARAM(TMessage(Message).lParam)) then
  begin
    Brush.Color := Render.GetBackGroundColor;
    Brush.Style := bsSolid;
    FillRect(Message.DC, ClientRect, Brush.Handle);
  end;
end;

procedure TStyledAnimatedButton.WMKillFocus(var Message: TMessage);
begin
  inherited;
  if AnimateOnFocus in FAutoAnimationTypes then
    AnimatedImage.StopAnimation;
end;

procedure TStyledAnimatedButton.WMSetFocus(var Message: TMessage);
begin
  if AnimateOnFocus in FAutoAnimationTypes then
      AnimatedImage.StartAnimation;
  inherited;
end;

procedure TStyledAnimatedButton.CMMouseEnter(var Message: TMessage);
begin
  if (FAnimateOnType = AnimateOnButton) and (AnimateOnMouseOver in FAutoAnimationTypes) then
    AnimatedImage.StartAnimation;
  inherited;
end;

procedure TStyledAnimatedButton.CMMouseLeave(var Message: TMessage);
begin
  if (FAnimateOnType = AnimateOnButton) and (AnimateOnMouseOver in FAutoAnimationTypes) then
    AnimatedImage.StopAnimation;
  inherited;
end;

procedure TStyledAnimatedButton.StartAnimation(ALoop: Boolean = False;
  AInverse: Boolean = False; AProgress: Double = 0);
begin
  AnimatedImage.SetLoop(ALoop);
  AnimatedImage.SetInverse(AInverse);
  AnimatedImage.InitAnimation(ALoop, AInverse, AProgress);
  AnimatedImage.StartAnimation;
end;

procedure TStyledAnimatedButton.AnimatedImageClick(Sender: TObject);
begin
  Self.Click;
end;

procedure TStyledAnimatedButton.AnimatedImageDblClick(Sender: TObject);
begin
  Self.DblClick;
end;

procedure TStyledAnimatedButton.AnimatedImageMouseEnter(Sender: TObject);
begin
  if (FAnimateOnType = AnimateOnImage) and (AnimateOnMouseOver in FAutoAnimationTypes) then
    AnimatedImage.StartAnimation;
end;

procedure TStyledAnimatedButton.AnimatedImageMouseLeave(Sender: TObject);
begin
  if (FAnimateOnType = AnimateOnImage) and (AnimateOnMouseOver in FAutoAnimationTypes) then
    AnimatedImage.StopAnimation;
end;

function TStyledAnimatedButton.AnimationIsRunning: Boolean;
begin
  Result := AnimatedImage.Animation.Running;
end;

procedure TStyledAnimatedButton.StopAnimation;
begin
  AnimatedImage.Animation.Stop;
  AnimatedImage.Animation.Progress := 1;
end;

procedure TStyledAnimatedButton.Paint;
var
  LImageRect: TRect;
  LImageList: TCustomImageList;
  LImageIndex: Integer;
  LImageWidth, LImageHeight: Integer;
begin
  if Assigned(FSkAnimatedImage) then
  begin
    Render.GetImageSize(LImageWidth, LImageHeight,
      LImageList, LImageIndex);
    if (LImageWidth=0) or (LImageHeight=0) then
    begin
      LImageWidth := Render.AnimationWidth;
      LImageHeight := Render.AnimationHeight;
    end;
    LImageRect := ClientRect;
    LImageRect := CalcImageRect(LImageRect, LImageWidth, LImageHeight);
    if (LImageRect.Width <> 0) and (LImageRect.Height <> 0) then
    begin
      FSkAnimatedImage.SetBounds(LImageRect.Left, LImageRect.Top,
        LImageRect.Width, LImageRect.Height);
      if not FSkAnimatedImage.AnimationRunning then
        FSkAnimatedImage.Animation.Progress := 1;
    end;
  end;
  inherited;
end;

procedure TStyledAnimatedButton.PauseAnimation;
begin
  AnimatedImage.PauseAnimation;
end;

procedure TStyledAnimatedButton.ResumeAnimation;
begin
  AnimatedImage.ResumeAnimation;
end;

procedure TStyledAnimatedButton.SetSource(
  const AValue: TSkAnimatedImage.TSource);
begin
  AnimatedImage.Source.Assign(AValue);
end;

procedure TStyledAnimatedButton.SetAnimateOnType(const AValue: TAnimateOnType);
begin
  FAnimateOnType := AValue;
end;

procedure TStyledAnimatedButton.SetAnimationHeight(const AValue: Integer);
begin
  Render.AnimationHeight := AValue;
end;

procedure TStyledAnimatedButton.SetAnimationInverse(const AValue: Boolean);
begin
  AnimatedImage.SetInverse(AValue);
end;

procedure TStyledAnimatedButton.SetAnimationLoop(const AValue: Boolean);
begin
  AnimatedImage.setloop(AValue);
end;

procedure TStyledAnimatedButton.SetAnimationWidth(const AValue: Integer);
begin
  Render.AnimationWidth := AValue;
end;

procedure TStyledAnimatedButton.SetAutoAnimationTypes(
  const AValue: TAutoAnimationTypes);
begin
  if FAutoAnimationTypes <> AValue then
  begin
    AnimatedImage.StopAnimation;
    FAutoAnimationTypes := AValue;
    if AnimateAlways in AutoAnimationTypes then
    begin
      AnimationLoop := True;
      AnimatedImage.StartAnimation(True);
    end;
  end;
end;

procedure TStyledAnimatedButton.SetAutoSizeAnimation(const AValue: Boolean);
begin
  Render.AutoSizeAnimation := AValue;
end;

procedure TStyledAnimatedButton.SetAutoSizeAnimationMargin(const AValue: Integer);
begin
  Render.AutoSizeAnimationMargin := AValue;
end;

procedure TStyledAnimatedButton.SetCursor(const AValue: TCursor);
begin
  inherited;
  //Same cursor of button for the internal component
  AnimatedImage.Cursor := AValue;
end;

end.
