{******************************************************************************}
{                                                                              }
{  StyledButtonInControlList: a Demo to show StyledGraphicButtons              }
{  inside a ControlList                                                        }
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
unit ControlListForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.BaseImageCollection,
  Vcl.VirtualImage, Vcl.ControlList, Vcl.StdCtrls,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList, Vcl.StyledButton,
  Vcl.ButtonStylesAttributes, Vcl.ImageCollection;

type
  TfmControlList = class(TForm)
    ControlList: TControlList;
    Label1: TLabel;
    VirtualImage1: TVirtualImage;
    Label2: TLabel;
    ControlListButton1: TControlListButton;
    ControlListButton2: TControlListButton;
    VirtualImageList: TVirtualImageList;
    StyledGraphicButton2: TStyledGraphicButton;
    StyledGraphicButton1: TStyledGraphicButton;
    ImageCollection: TImageCollection;
    procedure ControlListBeforeDrawItem(AIndex: Integer; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
    procedure StyledGraphicButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmControlList: TfmControlList;

implementation

{$R *.dfm}

uses
  Vcl.BootstrapButtonStyles
  , Vcl.StyledTaskDialog
  ;

procedure TfmControlList.ControlListBeforeDrawItem(AIndex: Integer;
  ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
var
  LButtonFamily: TStyledButtonFamily;
  LClasses: TButtonClasses;
  LIndex: Integer;
begin
  VirtualImage1.ImageIndex := AIndex;
  Label2.Caption := ImageCollection.GetNameByIndex(AIndex);
  Label1.Caption := ImageCollection.GetNameByIndex(AIndex);

  //Settings Styled button based on Index
  LButtonFamily := BOOTSTRAP_FAMILY;
  LClasses := GetButtonFamilyClasses(LButtonFamily);
  LIndex := AIndex mod Length(LClasses);
  StyledGraphicButton1.StyleFamily := LButtonFamily;
  StyledGraphicButton1.StyleClass := LClasses[LIndex];
  StyledGraphicButton1.StyleAppearance := BOOTSTRAP_NORMAL;
  StyledGraphicButton1.Caption := StyledGraphicButton1.StyleClass;
  StyledGraphicButton1.Tag := AIndex;
  StyledGraphicButton1.Invalidate;

  StyledGraphicButton2.StyleFamily := LButtonFamily;
  StyledGraphicButton2.StyleClass := LClasses[LIndex];
  StyledGraphicButton2.StyleAppearance := BOOTSTRAP_OUTLINE;
  StyledGraphicButton2.Caption := LClasses[LIndex];
  StyledGraphicButton2.Tag := AIndex;
  StyledGraphicButton2.Invalidate;

  ControlListButton1.ImageIndex := (AIndex mod 11);
  ControlListButton2.ImageIndex := (AIndex mod 11)+1;

end;

procedure TfmControlList.StyledGraphicButton2Click(Sender: TObject);
var
  LButton: TStyledGraphicButton;
begin
  LButton := TStyledGraphicButton(Sender);
  StyledShowMessage(Format('StyleGraphicButton: "%s" - Band n.%d',
    [LButton.Caption, LButton.Tag]));
end;

end.
