{******************************************************************************}
{                                                                              }
{  StyledToolbar: a Toolbar with TStyledAnimatedToolButtons inside             }
{  Based on TStyledToolbar and animations using Skia4Delphi                    }
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
unit Vcl.StyledAnimatedToolbar;

interface

{$INCLUDE StyledComponents.inc}

uses
  Vcl.StyledButton
  , Vcl.StyledAnimatedButton
  , Vcl.StyledToolbar
  , Winapi.Windows
  , Winapi.Messages
  , System.SysUtils
  , System.Types
  , System.Classes
  , System.UITypes
  , Vcl.Controls
  , Vcl.ImgList
  , Vcl.Graphics
  , Vcl.ButtonStylesAttributes
  , Vcl.Skia
  , Vcl.SkAnimatedImageHelper
  ;

type
  TStyledAnimatedToolbar = class;
  TStyledAnimatedToolButton = class;

  TAnimatedButtonProc = reference to procedure (Button: TStyledAnimatedToolButton);
  TControlProc = reference to procedure (Control: TControl);

  TStyledAnimatedToolButton = class(TStyledToolButton)
  strict private
    FSkAnimatedImage: TSkAnimatedImage;
    FAnimationSource: TSkAnimatedImage.TSource;
    procedure AnimatedImageClick(Sender: TObject);
    procedure AnimatedImageDblClick(Sender: TObject);
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
    procedure CMMouseEnter(var Message: TNotifyEvent); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TNotifyEvent); message CM_MOUSELEAVE;
    procedure ReadData(AStream: TStream);
    procedure WriteData(AStream: TStream);
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
    property AutoAnimationTypes: TAutoAnimationTypes read FAutoAnimationTypes write SetAutoAnimationTypes;
    property AutoSizeAnimation: Boolean read FAutoSizeAnimation write SetAutoSizeAnimation default True;
    property AutoSizeAnimationMargin: Integer read FAutoSizeAnimationMargin write SetAutoSizeAnimationMargin default DEFAULT_ANIM_MARGIN;
    property AnimationSource: TSkAnimatedImage.TSource read GetSource write SetSource;
    property AnimationWidth: Integer read FAnimationWidth write SetAnimationWidth default DEFAULT_ANIM_SIZE;
    property AnimationHeight: Integer read FAnimationHeight write SetAnimationHeight  default DEFAULT_ANIM_SIZE;
  end;

  TStyledAnimatedToolbar = class(TStyledToolbar)
  private
  protected
    function GetStyledToolButtonClass: TStyledToolButtonClass; override;
  public
  published
  end;

implementation

uses
  Vcl.Consts
  , System.IOUtils
  , System.math
  {$IFDEF D10_3+}
  , Vcl.VirtualImageList
  {$ENDIF}
  ;

{ TStyledAnimatedToolbar }

function TStyledAnimatedToolbar.GetStyledToolButtonClass: TStyledToolButtonClass;
begin
  Result := TStyledAnimatedToolButton;
end;

{ TStyledAnimatedToolButton }

procedure TStyledAnimatedToolButton.CreateAnimation;
begin
  FSkAnimatedImage := TSkAnimatedImage.Create(Self);
  //By default the animation is stopped on Progress 1 (last frame)
  FSkAnimatedImage.SetProgress(1);
  FSkAnimatedImage.Animation.Stop;
  FSkAnimatedImage.SetLoop(False);
  if Assigned(Self.Parent) then
    FSkAnimatedImage.Parent := Self.Parent;
  FSkAnimatedImage.OnClick := AnimatedImageClick;
  FSkAnimatedImage.OnDblClick := AnimatedImageDblClick;
  FSkAnimatedImage.Visible := False;
  FSkAnimatedImage.Cursor := Cursor;
end;

constructor TStyledAnimatedToolButton.CreateStyled(
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
end;

procedure TStyledAnimatedToolButton.LoadAnimationFromStream(
  const AStream: TStream);
var
  LBytes: TBytes;
begin
  SetLength(LBytes, AStream.Size - AStream.Position);
  if Length(LBytes) > 0 then
    AStream.ReadBuffer(LBytes, 0, Length(LBytes));
  FAnimationSource.Data := LBytes;
end;

procedure TStyledAnimatedToolButton.LoadAnimationFromFile(
  const AFileName: TFileName);
begin
  FAnimationSource.Data := TFile.ReadAllBytes(AFileName);
  ;
end;

procedure TStyledAnimatedToolButton.ReadData(AStream: TStream);
begin
  if AStream.Size = 0 then
    FAnimationSource.Data := nil
  else
    LoadAnimationFromStream(AStream);
end;

procedure TStyledAnimatedToolButton.WriteData(AStream: TStream);
begin
  if FAnimationSource.Data <> nil then
    AStream.WriteBuffer(FAnimationSource.Data,
      Length(FAnimationSource.Data));
end;

procedure TStyledAnimatedToolButton.DefineProperties(AFiler: TFiler);

  function DoWrite: Boolean;
  begin
    if AFiler.Ancestor <> nil then
      Result := (not (AFiler.Ancestor is TStyledAnimatedButton)) or
        not TStyledAnimatedButton(AFiler.Ancestor).AnimationSource.Equals(FAnimationSource)
    else
      Result := FAnimationSource.Data <> nil;
  end;

begin
  inherited;
  AFiler.DefineBinaryProperty('AnimationData', ReadData, WriteData, DoWrite);
end;

destructor TStyledAnimatedToolButton.Destroy;
begin
  inherited;
end;

function TStyledAnimatedToolButton.GetAnimatedImage: TSkAnimatedImage;
begin
  if not Assigned(FSkAnimatedImage) then
    CreateAnimation;
  Result := FSkAnimatedImage;
end;

function TStyledAnimatedToolButton.GetRenderClass: TStyledButtonRenderClass;
begin
  Result := TStyledAnimatedButtonRender;
end;

function TStyledAnimatedToolButton.GetSource: TSkAnimatedImage.TSource;
begin
  Result := FAnimationSource;
end;

procedure TStyledAnimatedToolButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
(*
  if (Operation = opInsert) and (AComponent = Images) then
    StopAnimation;
*)
  inherited;
end;

function TStyledAnimatedToolButton.CanPlayAnimation: boolean;
begin
  Result := Length(FAnimationSource.Data) > 0;
end;

procedure TStyledAnimatedToolButton.Click;
begin
  if AnimateOnClick in FAutoAnimationTypes then
    AnimatedImage.StartAnimation;
  inherited;
end;

procedure TStyledAnimatedToolButton.WMSetFocus(var Message: TMessage);
begin
  if AnimateOnFocus in FAutoAnimationTypes then
      AnimatedImage.StartAnimation;
  inherited;
end;

procedure TStyledAnimatedToolButton.CMMouseEnter(var Message: TNotifyEvent);
begin
  if AnimateOnMouseOver in FAutoAnimationTypes then
    AnimatedImage.StartAnimation;
  inherited;
end;

procedure TStyledAnimatedToolButton.CMMouseLeave(var Message: TNotifyEvent);
begin
  if AnimateOnMouseOver in FAutoAnimationTypes then
    AnimatedImage.StopAnimation;
  inherited;
end;

procedure TStyledAnimatedToolButton.StartAnimation(ALoop: Boolean = False;
  AInverse: Boolean = False; AProgress: Double = 0);
begin
  AnimatedImage.SetLoop(ALoop);
  AnimatedImage.SetInverse(AInverse);
  AnimatedImage.InitAnimation(ALoop, AInverse, AProgress);
  AnimatedImage.StartAnimation;
end;

procedure TStyledAnimatedToolButton.AnimatedImageClick(Sender: TObject);
begin
  Self.Click;
end;

procedure TStyledAnimatedToolButton.AnimatedImageDblClick(Sender: TObject);
begin
  Self.DblClick;
end;

function TStyledAnimatedToolButton.AnimationIsRunning: Boolean;
begin
  Result := AnimatedImage.Animation.Running;
end;

procedure TStyledAnimatedToolButton.StopAnimation;
begin
  AnimatedImage.Animation.Stop;
  AnimatedImage.Animation.Progress := 1;
end;

procedure TStyledAnimatedToolButton.Paint;
(*
var
  LImageRect: TRect;
  LImageList: TCustomImageList;
  LImageIndex: Integer;
  LImageWidth, LImageHeight: Integer;
*)
begin
(*
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
*)
    inherited;
end;

procedure TStyledAnimatedToolButton.PauseAnimation;
begin
  AnimatedImage.PauseAnimation;
end;

procedure TStyledAnimatedToolButton.ResumeAnimation;
begin
  AnimatedImage.ResumeAnimation;
end;

procedure TStyledAnimatedToolButton.SetSource(
  const AValue: TSkAnimatedImage.TSource);
begin
  FAnimationSource.Assign(AValue);
end;

procedure TStyledAnimatedToolButton.SetAnimationHeight(const AValue: Integer);
begin
  if FAnimationHeight <> AValue then
  begin
    FAnimationHeight := AValue;
    FAutoSizeAnimation := False;
    Invalidate;
  end;
end;

procedure TStyledAnimatedToolButton.SetAnimationWidth(const AValue: Integer);
begin
  if FAnimationWidth <> AValue then
  begin
    FAnimationWidth := AValue;
    FAutoSizeAnimation := False;
    Invalidate;
  end;
end;

procedure TStyledAnimatedToolButton.SetAutoAnimationTypes(
  const AValue: TAutoAnimationTypes);
begin
  if FAutoAnimationTypes <> AValue then
  begin
    FAutoAnimationTypes := AValue;
  end;
end;

procedure TStyledAnimatedToolButton.SetAutoSizeAnimation(const AValue: Boolean);
begin
  if FAutoSizeAnimation <> AValue then
  begin
    FAutoSizeAnimation := AValue;
    Invalidate;
  end;
end;

procedure TStyledAnimatedToolButton.SetAutoSizeAnimationMargin(const AValue: Integer);
begin
  if FAutoSizeAnimationMargin <> AValue then
  begin
    FAutoSizeAnimationMargin := AValue;
    Invalidate;
  end;
end;

procedure TStyledAnimatedToolButton.SetCursor(const AValue: TCursor);
begin
  inherited;
  //Same cursor of button for the internal component
  AnimatedImage.Cursor := AValue;
end;

end.
