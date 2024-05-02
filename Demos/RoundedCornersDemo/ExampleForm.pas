{******************************************************************************}
{                                                                              }
{  StyledRoundedCorners Examples                                               }
{  Using StyledRoundedCorners options for Styled Components                    }
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
unit ExampleForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ButtonStylesAttributes,
  Vcl.StyledButtonEditorUnit,
  Vcl.StyledButton, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TRoundedCornerExamples = class(TForm)
    TopLeftBottomRightButton: TStyledButton;
    NoTopLeftButton: TStyledButton;
    NoTopRightButton: TStyledButton;
    NoTopButton: TStyledButton;
    NoLeftButton: TStyledButton;
    NoRightButton: TStyledButton;
    NoBottomButton: TStyledButton;
    NoBottomLeftButton: TStyledButton;
    NoBottomRightButton: TStyledButton;
    BottomLeftTopRightButton: TStyledButton;
    RoundedButton: TStyledButton;
    NoRoundedButton: TStyledButton;
    StylePanel: TPanel;
    StyleLabel: TLabel;
    cbChangeStyle: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure cbChangeStyleSelect(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
  private
    procedure BuildStyleList;
  public
  end;

var
  RoundedCornerExamples: TRoundedCornerExamples;

implementation

{$R *.dfm}

uses
  Vcl.Themes
  ;

procedure TRoundedCornerExamples.BuildStyleList;
var
  i, SelectedIndex: integer;
  LStyleName, LActiveStyleName: string;
begin
  SelectedIndex := -1;
  cbChangeStyle.Items.Clear;
  LActiveStyleName := TStyleManager.ActiveStyle.Name;
  for i := 0 to High(TStyleManager.StyleNames) do
  begin
    LStyleName := TStyleManager.StyleNames[i];
    cbChangeStyle.Items.Add(LStyleName);
    if SameText(LStyleName, LActiveStyleName)  then
      SelectedIndex := i;
  end;
  cbChangeStyle.ItemIndex := SelectedIndex;
end;

procedure TRoundedCornerExamples.ButtonClick(Sender: TObject);
begin
  EditStyledButton(Sender as TStyledButton);
end;

procedure TRoundedCornerExamples.cbChangeStyleSelect(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    TStyleManager.TrySetStyle(cbChangeStyle.Text);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TRoundedCornerExamples.FormCreate(Sender: TObject);
begin
  BuildStyleList;
  Caption := Application.Title;
end;

end.
