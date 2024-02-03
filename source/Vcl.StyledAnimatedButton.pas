{******************************************************************************}
{                                                                              }
{       TStyledAnimatedButton: a StyledButton with support for Skia Animations }
{                                                                              }
{       Copyright (c) 2022-2024 (Ethea S.r.l.)                                 }
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
  TAutoAnimationType = (AnimateOnMouseOver, AnimateOnClick, AnimateAlways, AnimateOnFocused);
  TAutoAnimationTypes = set of TAutoAnimationType;

  TStyledAnimatedButtonRender = class(TStyledButtonRender)
  public
    function GetImageSize(out AWidth: Integer; out AHeight: Integer; out AImageList: TCustomImageList; out AImageIndex: Integer): Boolean; override;
  end;

  TStyledAnimatedButton = class(TStyledButton)
  strict private
    FSkAnimatedImage: TSkAnimatedImage;
    procedure AnimatedImageClick(Sender: TObject);
    procedure AnimatedImageDblClick(Sender: TObject);
    procedure AnimatedImageMouseEnter(Sender: TObject);
    procedure AnimatedImageMouseLeave(Sender: TObject);
  private
    FAnimationHeight: Integer;
    FAnimationWidth: Integer;
    FAutoSizeAnimation: Boolean;
    FAutoSizeAnimationMargin: Integer;
    FAutoAnimationTypes: TAutoAnimationTypes;
    function GetAnimatedImage: TSkAnimatedImage;
    function GetSource: TSkAnimatedImage.TSource;
    procedure SetSource(const AValue: TSkAnimatedImage.TSource);
    procedure SetAnimationHeight(const AValue: Integer);
    procedure SetAnimationWidth(const AValue: Integer);
    procedure SetAutoSizeAnimation(const AValue: Boolean);
    procedure SetAutoSizeAnimationMargin(const AValue: Integer);
    procedure SetAutoAnimationTypes(const AValue: TAutoAnimationTypes);
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure ReadData(AStream: TStream);
    procedure WriteData(AStream: TStream);
    procedure SetAnimationInverse(const AValue: Boolean);
    procedure SetAnimationLoop(const AValue: Boolean);
    function GetAnimationInverse: Boolean;
    function GetAnimationLoop: Boolean;
  strict protected
    procedure DefineProperties(AFiler: TFiler); override;
  protected
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
    procedure Click; override;
  public
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
  published
    property AutoAnimationTypes: TAutoAnimationTypes read FAutoAnimationTypes write SetAutoAnimationTypes default [AnimateOnMouseOver];
    property AutoSizeAnimation: Boolean read FAutoSizeAnimation write SetAutoSizeAnimation default True;
    property AutoSizeAnimationMargin: Integer read FAutoSizeAnimationMargin write SetAutoSizeAnimationMargin default DEFAULT_ANIM_MARGIN;
    property AnimationSource: TSkAnimatedImage.TSource read GetSource write SetSource;
    property AnimationWidth: Integer read FAnimationWidth write SetAnimationWidth default DEFAULT_ANIM_SIZE;
    property AnimationHeight: Integer read FAnimationHeight write SetAnimationHeight  default DEFAULT_ANIM_SIZE;
    property AnimationLoop: Boolean read GetAnimationLoop write SetAnimationLoop;
    property AnimationInverse: Boolean read GetAnimationInverse write SetAnimationInverse;
  end;

implementation

uses
  System.Skia
  , System.math
  {$IFDEF D10_3+}
  , Vcl.VirtualImageList
  {$ENDIF}
  ;

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
  FAnimationHeight := DEFAULT_ANIM_SIZE;
  FAnimationWidth := DEFAULT_ANIM_SIZE;
  FAutoSizeAnimationMargin := DEFAULT_ANIM_MARGIN;
  FAutoSizeAnimation := True;
  FAutoAnimationTypes := [AnimateOnMouseOver];
end;

procedure TStyledAnimatedButton.ReadData(AStream: TStream);
begin
  if AStream.Size = 0 then
    FSkAnimatedImage.Source.Data := nil
  else
    FSkAnimatedImage.LoadFromStream(AStream);
end;

procedure TStyledAnimatedButton.WriteData(AStream: TStream);
begin
  if FSkAnimatedImage.Source.Data <> nil then
    AStream.WriteBuffer(FSkAnimatedImage.Source.Data,
      Length(FSkAnimatedImage.Source.Data));
end;

procedure TStyledAnimatedButton.DefineProperties(AFiler: TFiler);

  function DoWrite: Boolean;
  begin
    if AFiler.Ancestor <> nil then
      Result := (not (AFiler.Ancestor is TStyledAnimatedButton)) or
        not TStyledAnimatedButton(AFiler.Ancestor).AnimationSource.Equals(FSkAnimatedImage.Source)
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

function TStyledAnimatedButton.GetAnimationInverse: Boolean;
begin
  Result := AnimatedImage.GetInverse;
end;

function TStyledAnimatedButton.GetAnimationLoop: Boolean;
begin
  Result := AnimatedImage.GetLoop;
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

procedure TStyledAnimatedButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
(*
  if (Operation = opInsert) and (AComponent = Images) then
    StopAnimation;
*)
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

procedure TStyledAnimatedButton.WMKillFocus(var Message: TMessage);
begin
  if AnimateOnFocused in FAutoAnimationTypes then
      AnimatedImage.StopAnimation;
end;

procedure TStyledAnimatedButton.WMSetFocus(var Message: TMessage);
begin
  if AnimateOnFocused in FAutoAnimationTypes then
      AnimatedImage.StartAnimation;
  inherited;
end;

procedure TStyledAnimatedButton.CMMouseEnter(var Message: TMessage);
begin
//  if AnimateOnMouseOver in FAutoAnimationTypes then
//    AnimatedImage.StartAnimation;
  inherited;
end;

procedure TStyledAnimatedButton.CMMouseLeave(var Message: TMessage);
begin
//  if AnimateOnMouseOver in FAutoAnimationTypes then
//    AnimatedImage.StopAnimation;
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
  if AnimateOnMouseOver in FAutoAnimationTypes then
    AnimatedImage.StartAnimation;
end;

procedure TStyledAnimatedButton.AnimatedImageMouseLeave(Sender: TObject);
begin
  if AnimateOnMouseOver in FAutoAnimationTypes then
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
      LImageWidth := FAnimationWidth;
      LImageHeight := FAnimationHeight;
    end;
    LImageRect := ClientRect;
    LImageRect := CalcImageRect(LImageRect, LImageWidth, LImageHeight);
    if (LImageRect.Width <> 0) and (LImageRect.Height <> 0) then
    begin
      FSkAnimatedImage.SetBounds(LImageRect.Left, LImageRect.Top, LImageRect.Width, LImageRect.Height);
      if not FSkAnimatedImage.AnimationRunning then
        FSkAnimatedImage.Animation.Progress := 1;
    end;
  end
  else
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

procedure TStyledAnimatedButton.SetAnimationHeight(const AValue: Integer);
begin
  if FAnimationHeight <> AValue then
  begin
    FAnimationHeight := AValue;
    FAutoSizeAnimation := False;
    Invalidate;
  end;
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
  if FAnimationWidth <> AValue then
  begin
    FAnimationWidth := AValue;
    FAutoSizeAnimation := False;
    Invalidate;
  end;
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
  if FAutoSizeAnimation <> AValue then
  begin
    FAutoSizeAnimation := AValue;
    Invalidate;
  end;
end;

procedure TStyledAnimatedButton.SetAutoSizeAnimationMargin(const AValue: Integer);
begin
  if FAutoSizeAnimationMargin <> AValue then
  begin
    FAutoSizeAnimationMargin := AValue;
    Invalidate;
  end;
end;

procedure TStyledAnimatedButton.SetCursor(const AValue: TCursor);
begin
  inherited;
  //Same cursor of button for the internal component
  AnimatedImage.Cursor := AValue;
end;

{ TStyledAnimatedButtonRender }

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
        AWidth := LSize - (LStyledAnimatedButton.FAutoSizeAnimationMargin * 2);
        AHeight := AWidth;
      end
      else
      begin
        AWidth := LStyledAnimatedButton.FAnimationWidth;
        AHeight := LStyledAnimatedButton.FAnimationHeight;
      end;
      Result := True;
    end;
  end;
end;

end.
