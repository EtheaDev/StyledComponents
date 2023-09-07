{******************************************************************************}
{                                                                              }
{       StyledTaskDialogStd: an example of Task Dialog Form                    }
{       using a TSkAnimatedImage with Lottie Animations                        }
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
  , Vcl.StyledTaskDialogFormUnit
  , Vcl.ExtCtrls
  , Vcl.StdCtrls
  , System.UITypes
  , Vcl.Skia
  ;

type
  TStyledTaskDialogAnimated = class(TStyledTaskDialogForm)
    SkAnimatedImage: TSkAnimatedImage;
  private
  protected
    procedure LoadImage(const AImageIndex: TImageIndex; AImageName: string); override;
  public
  end;

implementation

{$R *.dfm}

uses
  Vcl.Themes;

procedure TStyledTaskDialogAnimated.LoadImage(
  const AImageIndex: TImageIndex; AImageName: string);
var
  LStream: TResourceStream;
  LImageName: string;
begin
  //Using ..\Animations\Animations.rc file compiled into Animations.RES file
  LImageName := UpperCase(Format('LOTTIE_%s',[AImageName]));
  LStream := TResourceStream.Create(HInstance, LImageName, RT_RCDATA);
  try
    SkAnimatedImage.LoadFromStream(LStream);
    SkAnimatedImage.Animation.Loop := False;
    SkAnimatedImage.Animation.Start;
  finally
    LStream.Free;
  end;
end;

initialization
  RegisterTaskDialogFormClass(TStyledTaskDialogAnimated);

end.
