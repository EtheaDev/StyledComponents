{******************************************************************************}
{                                                                              }
{  StyledTaskDialogAnimated: an example of Task Dialog Form                    }
{  using a TSkAnimatedImage with Lottie Animations                             }
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
unit Skia.Vcl.StyledTaskDialogAnimatedUnit;

interface

{$R Animations.RES}
{$INCLUDE StyledComponents.inc}

uses
  Winapi.Windows
  , Winapi.Messages
  , System.SysUtils
  , System.Variants
  , System.Classes
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.StyledButton
  , Vcl.StandardButtonStyles
  , Vcl.BootstrapButtonStyles
  , Vcl.ButtonStylesAttributes
  , Vcl.AngularButtonStyles
  , Vcl.ColorButtonStyles
  , Vcl.StyledTaskDialog
  , Vcl.StyledTaskDialogFormUnit
  , Vcl.ExtCtrls
  , Vcl.StdCtrls
  , System.UITypes
  , Vcl.Skia //Warning: you cannot use Animated Style Dialog if you don't have Skia4Delphi!
  , System.Skia
  ;

type
  TStyledTaskDialogAnimatedForm = class(TStyledTaskDialogForm)
    SkFooterAnimatedImage: TSkAnimatedImage;
    SkAnimatedImage: TSkAnimatedImage;
  private
    procedure InternalLoadImage(const AAnimatedImage: TSkAnimatedImage;
      const AImageIndex: TImageIndex; AImageName: string);
    function TaskDialogIconToIconName(
      const ATaskDialogIcon: TTaskDialogIcon): string;
  protected
    class function CanUseAnimations: Boolean; override;
    procedure LoadImage(const AImageIndex: TImageIndex; AImageName: string); override;
    procedure LoadCustomMainIcon(const AIcon: TIcon;
      const ATaskDialogIcon: TTaskDialogIcon); override;
    procedure LoadCustomFooterIcon(const AIcon: TIcon;
      const ATaskDialogIcon: TTaskDialogIcon); override;
  public
  end;

implementation

{$R *.dfm}

uses
  Vcl.Themes;

class function TStyledTaskDialogAnimatedForm.CanUseAnimations: Boolean;
begin
  Result := True;
end;

function TStyledTaskDialogAnimatedForm.TaskDialogIconToIconName(
  const ATaskDialogIcon: TTaskDialogIcon): string;
begin
  case ATaskDialogIcon of
    tdiWarning: Result := 'Warning';
    tdiError: Result := 'Error';
    tdiInformation: Result := 'Information';
    tdiShield: Result := 'Shield';
  else
    Result := 'Custom';
  end;
end;

procedure TStyledTaskDialogAnimatedForm.LoadCustomFooterIcon(const AIcon: TIcon;
  const ATaskDialogIcon: TTaskDialogIcon);
var
  LImageName: string;
begin
  inherited;
  //A Custom FooterIcon is not animated so ignore It
  LImageName := TaskDialogIconToIconName(ATaskDialogIcon);
  if LImageName <> '' then
    InternalLoadImage(SkFooterAnimatedImage, -1, LImageName);
end;

procedure TStyledTaskDialogAnimatedForm.LoadCustomMainIcon(const AIcon: TIcon;
  const ATaskDialogIcon: TTaskDialogIcon);
var
  LImageName: string;
begin
  //A Custom MainIcon is not animated so ignore it
  LImageName := TaskDialogIconToIconName(ATaskDialogIcon);
  if LImageName <> '' then
    InternalLoadImage(SkAnimatedImage, -1, LImageName);
end;

procedure TStyledTaskDialogAnimatedForm.LoadImage(
  const AImageIndex: TImageIndex; AImageName: string);
begin
  InternalLoadImage(SkAnimatedImage, AImageIndex, AImageName);
end;

procedure TStyledTaskDialogAnimatedForm.InternalLoadImage(
  const AAnimatedImage: TSkAnimatedImage;
  const AImageIndex: TImageIndex; AImageName: string);
var
  LStream: TResourceStream;
  LImageName: string;
begin
  //Using ..\Animations\Animations.rc file compiled into Animations.RES file
  LImageName := UpperCase('LOTTIE_'+AImageName);
  LStream := TResourceStream.Create(HInstance, LImageName, RT_RCDATA);
  try
    AAnimatedImage.LoadFromStream(LStream);
    AAnimatedImage.Animation.Loop := False;
    AAnimatedImage.Animation.Start;
  finally
    LStream.Free;
  end;
end;

initialization
  RegisterTaskDialogFormClass(TStyledTaskDialogAnimatedForm);
  InitializeStyledTaskDialogs(True);

finalization
  UnregisterTaskDialogFormClass(TStyledTaskDialogAnimatedForm);

end.
