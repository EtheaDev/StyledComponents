{******************************************************************************}
{                                                                              }
{  StyledTaskDialogStd: an example of Task Dialog Form                         }
{  using an ImageList and a Image component                                    }
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
unit Vcl.StyledTaskDialogStdUnit;

interface

{$R IconsPNG.RES}
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
  , Vcl.StyledTaskDialogFormUnit
  , Vcl.ImgList
  , Vcl.StyledButton
  , Vcl.ExtCtrls
  , Vcl.StdCtrls
  , System.UITypes
  , Vcl.Imaging.pngimage
  , Vcl.ButtonStylesAttributes
  , Vcl.StandardButtonStyles
  , Vcl.BootstrapButtonStyles
  , Vcl.AngularButtonStyles
  , Vcl.ColorButtonStyles
  ;

type
  TStyledTaskDialogStd = class(TStyledTaskDialogForm)
    FooterImage: TImage;
    Image: TImage;
  private
  protected
    procedure InternalLoadImage(const AImage: TImage;
      const AImageIndex: TImageIndex; AImageName: string); virtual;
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

class function TStyledTaskDialogStd.CanUseAnimations: Boolean;
begin
  Result := False;
end;

procedure TStyledTaskDialogStd.LoadCustomFooterIcon(const AIcon: TIcon;
  const ATaskDialogIcon: TTaskDialogIcon);
begin
  if (ATaskDialogIcon <> tdiNone) and not Assigned(AIcon) then
    InternalLoadImage(FooterImage,
      TaskDialogIconToImageIndex(ATaskDialogIcon), '')
  else if Assigned(AIcon) then
    FooterImage.Picture.Bitmap.Assign(AIcon);
end;

procedure TStyledTaskDialogStd.LoadCustomMainIcon(const AIcon: TIcon;
  const ATaskDialogIcon: TTaskDialogIcon);
begin
  if (ATaskDialogIcon <> tdiNone) and not Assigned(AIcon) then
    InternalLoadImage(Image,
      TaskDialogIconToImageIndex(ATaskDialogIcon), '')
  else if Assigned(AIcon) then
    Image.Picture.Bitmap.Assign(AIcon);
end;

procedure TStyledTaskDialogStd.LoadImage(
  const AImageIndex: TImageIndex; AImageName: string);
begin
  InternalLoadImage(Image, AImageIndex, AImageName);
end;

procedure TStyledTaskDialogStd.InternalLoadImage(const AImage: TImage;
  const AImageIndex: TImageIndex; AImageName: string);
var
  LStream: TResourceStream;
  LImageName: string;
  LPngImage: TPngImage;
begin
  if AImageName = '' then
    Exit;
  //Using IconsPNG.res
  LImageName := UpperCase('STYLEDTASKICON_'+AImageName);
  LStream := TResourceStream.Create(HInstance, LImageName, RT_RCDATA);
  try
    LPngImage := TPngImage.Create;
    try
      LPngImage.LoadFromStream(LStream);
      AImage.Picture.Assign(LPngImage);
    finally
      LPngImage.Free;
    end;
  finally
    LStream.Free;
  end;
end;

end.
