{******************************************************************************}
{                                                                              }
{  SkAnimatedImageHelper: an helper class for TSkAnimatedImage                 }
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
unit Vcl.SkAnimatedImageHelper;

interface

{$INCLUDE StyledComponents.inc}

uses
  Winapi.Windows
  , Winapi.Messages
  , System.Types
  , System.Classes
(*
  , System.UITypes
  , Vcl.Controls
  , Vcl.ImgList
*)
  , Vcl.Graphics
  , Vcl.Skia
  ;

type
  TSkAnimatedImageHelper = class helper for TSkAnimatedImage
  strict private
    function AnimationLoaded: Boolean;
  public
    //Initialization functions
    procedure InitAnimation(ALoop, AInverse: Boolean;
      AProgress: Double);
    procedure ClearAnimationData;

    function CanPlayAnimation: Boolean;
    function CanResumeAnimation: Boolean;
    function CanPauseAnimation: Boolean;
    function CanStopAnimation: Boolean;
    function AnimationRunning: Boolean;
    function AnimationRunningInverse: Boolean;
    function AnimationRunningNormal: Boolean;

    function GetProgress: Double;
    function GetProgressPercentage: SmallInt;
    procedure SetProgress(const AValue: Double);
    procedure SetProgressPercentage(const AValue: SmallInt);

    function GetLoop: Boolean;
    procedure SetLoop(const Value: Boolean);
    function GetInverse: Boolean;
    procedure SetInverse(const Value: Boolean);

    function GetLottieText: string;

    procedure StopAnimation;
    procedure PauseAnimation;
    procedure ResumeAnimation;
    procedure StartAnimation(const AFromBegin: Boolean = True);

    procedure RenderTo(ACanvas: TCanvas; ARect: TRectF;
      AProgress: Double; AOpacity: Single);
  end;

implementation

uses
  System.Skia
  , System.SysUtils
  , System.math
  ;

{ TSkAnimatedImageHelper }

function TSkAnimatedImageHelper.AnimationLoaded: Boolean;
begin
  Result := (Length(Source.Data) > 0) and Assigned(Codec) and
    not Codec.Isstatic;
end;

procedure TSkAnimatedImageHelper.InitAnimation(ALoop, AInverse: Boolean;
  AProgress: Double);
begin
  Animation.Loop := ALoop;
  Animation.Inverse := AInverse;
  Animation.Progress := AProgress;
end;

function TSkAnimatedImageHelper.AnimationRunning: Boolean;
begin
  Result := AnimationLoaded and Animation.Enabled and Animation.Running;
end;

function TSkAnimatedImageHelper.AnimationRunningInverse: Boolean;
begin
  Result := AnimationRunning and Animation.Inverse;
end;

function TSkAnimatedImageHelper.AnimationRunningNormal: Boolean;
begin
  Result := AnimationRunning and not Animation.Inverse;
end;

function TSkAnimatedImageHelper.CanPlayAnimation: Boolean;
begin
  Result := AnimationLoaded and not Animation.Running;
end;

function TSkAnimatedImageHelper.CanResumeAnimation: Boolean;
begin
  Result := AnimationLoaded and not AnimationRunning and
    (Animation.Progress <> 0) and
    (Animation.Loop or (Animation.Progress <> 1));
end;

function TSkAnimatedImageHelper.CanPauseAnimation: Boolean;
begin
  Result := AnimationRunning;
end;

function TSkAnimatedImageHelper.CanStopAnimation: Boolean;
begin
  Result := AnimationRunning;
end;

procedure TSkAnimatedImageHelper.ClearAnimationData;
var
  LEmpty: TBytes;
begin
  SetLength(LEmpty,0);
  Source.Data := LEmpty;
end;

function TSkAnimatedImageHelper.GetProgressPercentage: SmallInt;
begin
  if AnimationLoaded then
    Result := Round(inherited Animation.Progress * 100)
  else
    Result := 0;
end;

procedure TSkAnimatedImageHelper.SetProgress(const AValue: Double);
var
  LEvent: TNotifyEvent;
begin
  if Assigned(Animation) then
  begin
    if Assigned(OnAnimationProcess) then
      LEvent := OnAnimationProcess
    else
      LEvent := nil;
    OnAnimationProcess := nil;
    Try
      Animation.Stop;
      Animation.Progress := AValue;
    Finally
      if Assigned(LEvent) then
        OnAnimationProcess := LEvent;
    End;
  end;
end;

function TSkAnimatedImageHelper.GetLottieText: string;
var
  LFormat: TFormatInfo;
  LStringStream: TStringStream;
begin
  Result := '';
  if AnimationLoaded then
  begin
    if Codec.TryDetectFormat(Source.Data, LFormat) then
    begin
      if SameText(LFormat.Name, 'Lottie') then
      begin
        LStringStream := TStringStream.Create('', TEncoding.UTF8);
        try
          LStringStream.writebuffer(Source.Data[0], length(Source.Data));
          Result := LStringStream.DataString;
        finally
          LStringStream.Free;
        end;
      end;
    end;
  end;
end;

function TSkAnimatedImageHelper.GetProgress: Double;
begin
  if AnimationLoaded then
    Result := inherited Animation.Progress
  else
    Result := 0;
end;

procedure TSkAnimatedImageHelper.SetProgressPercentage(const AValue: SmallInt);
begin
  if Assigned(Animation) then
    Animation.Progress := AValue / 100;
end;

function TSkAnimatedImageHelper.GetLoop: Boolean;
begin
  Result := Assigned(Animation) and Animation.Loop;
end;

procedure TSkAnimatedImageHelper.SetLoop(const Value: Boolean);
begin
  Animation.Loop := Value;
end;

function TSkAnimatedImageHelper.GetInverse: Boolean;
begin
  Result := Assigned(Animation) and Animation.Inverse;
end;

procedure TSkAnimatedImageHelper.SetInverse(const Value: Boolean);
begin
  if Assigned(Animation) then
    Animation.Inverse := Value;
end;


procedure TSkAnimatedImageHelper.StopAnimation;
begin
  if Assigned(Animation) then
    Animation.Stop;
end;

procedure TSkAnimatedImageHelper.PauseAnimation;
begin
  if Assigned(Animation) then
    Animation.StopAtCurrent;
end;

procedure TSkAnimatedImageHelper.ResumeAnimation;
begin
  if Assigned(Animation) then
  begin
    Animation.StartFromCurrent := Animation.Progress <> 1;
    Animation.start;
  end;
end;


procedure TSkAnimatedImageHelper.StartAnimation(const AFromBegin: Boolean = True);
begin
  if Assigned(Animation) then
  begin
    Animation.StartFromCurrent := not AFromBegin;
    if AFromBegin then
    begin
      if Animation.Inverse then
        inherited Animation.Progress := 1
      else
        inherited Animation.Progress := 0;
    end
    else
    begin
      if not Animation.Inverse and (Animation.Progress = 1) then
        inherited Animation.Progress := 0
      else if Animation.Inverse and (Animation.Progress = 0) then
        inherited Animation.Progress := 1;
    end;
    if AnimationLoaded and not Animation.Running then
      Animation.Start;
  end;
end;

procedure TSkAnimatedImageHelper.RenderTo(ACanvas: TCanvas; ARect: TRectF;
  AProgress: Double; AOpacity: Single);
var
  LBitmap: TBitmap;
  LRect: TRectF;
  LTop, LLeft, LWidth, LHeight: Integer;
begin
  ACanvas.Lock;
  try
    Animation.Progress := AProgress;
    LTop := Round(ARect.Top);
    LLeft := Round(ARect.Left);
    LWidth := Round(ARect.Width);
    LHeight := Round(ARect.Height);
    LBitmap := TBitmap.Create;
    try
      LBitmap.PixelFormat := pf32bit;
      LBitmap.AlphaFormat := afPremultiplied;
      LBitmap.SetSize(LWidth, LHeight);
      LRect := TRectF.Create(0,0,LWidth, LHeight);
      LBitmap.SkiaDraw(
        procedure (const ASkCanvas: ISkCanvas)
        begin
          RenderFrame(ASkCanvas, LRect, AProgress, AOpacity);
        end
        );
        ACanvas.Draw(LTop, LLeft, LBitmap);
    finally
      LBitmap.Free;
    end;
  finally
    ACanvas.Unlock;
  end;
end;

end.
