{******************************************************************************}
{                                                                              }
{  StyledTaskDialogStd: an example of Task Dialog Form                         }
{  using an ImageList and a Image component                                    }
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
unit Vcl.StyledTaskDialogStdUnit;

interface

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
    ImageList: TImageList;
    Image: TImage;
  private
  protected
    class function CanUseAnimations: Boolean; override;
    procedure LoadImage(const AImageIndex: TImageIndex; AImageName: string); override;
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

procedure TStyledTaskDialogStd.LoadImage(
  const AImageIndex: TImageIndex; AImageName: string);
var
  LBackGroundColor: TColor;
begin
  LBackGroundColor := TStyleManager.ActiveStyle.GetSystemColor(clWindow);
  Image.Picture.Bitmap.Canvas.Brush.Color := LBackGroundColor;
  Image.Picture.Bitmap.Canvas.FillRect(Rect(0, 0,
    ImageList.Width, ImageList.Height));
  Image.Picture.Bitmap.PixelFormat := pf32bit;
  Image.Picture.Bitmap.Transparent := True;
  ImageList.GetBitmap(AImageIndex, Image.Picture.Bitmap);
end;

initialization
  RegisterTaskDialogFormClass(TStyledTaskDialogStd);

end.
