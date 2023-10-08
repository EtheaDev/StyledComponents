{******************************************************************************}
{                                                                              }
{       TStyledDbNavigator Test                                                }
{       Comparison TStyledDbNavigator with TDbNavigator                        }
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
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ButtonStylesAttributes,
  Vcl.StandardButtonStyles, Vcl.AngularButtonStyles, Vcl.BootstrapButtonStyles, Vcl.ColorButtonStyles,
  Vcl.StyledButton, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ToolWin, Vcl.ActnMan,
  Vcl.ActnCtrls, Vcl.ImgList, Vcl.StdCtrls, Vcl.StyledDbNavigator, Vcl.Menus,
  Vcl.DBCtrls, Data.DB, Vcl.Grids, Vcl.DBGrids, Datasnap.DBClient, Vcl.Buttons,
  System.ImageList;

const
  //Params to check
  FLAT_BUTTONS = False;
  DBNAV_HEIGHT = 50;
  DBNAV_WIDTH = 50;
  SHOW_CAPTIONS = False;

  //Test alignment
  //DBNAV_ALIGN = alTop;
  //DBNAV_KIND = dbnHorizontal;
  DBNAV_ALIGN = alLeft;
  DBNAV_KIND = dbnVertical;

type
  TfmMain = class(TForm)
    BottomPanel: TPanel;
    ImageList32: TImageList;
    ClientPanel: TPanel;
    LeftPanel: TPanel;
    CreateButton: TStyledButton;
    BottomClientPanel: TPanel;
    WidthLabel: TLabel;
    HeightLabel: TLabel;
    tbWidth: TTrackBar;
    tbHeight: TTrackBar;
    ShowCaptionsCheckBox: TCheckBox;
    FlatCheckBox: TCheckBox;
    PopupMenu: TPopupMenu;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Exit1: TMenuItem;
    Panel1: TPanel;
    cbChangeStyle: TComboBox;
    StyleLabel: TLabel;
    DBNavigator: TDBNavigator;
    ClientDataSet: TClientDataSet;
    ClientDataSetID: TAutoIncField;
    ClientDataSetCustomerID: TIntegerField;
    ClientDataSetProductID: TIntegerField;
    ClientDataSetPurchaseDate: TDateTimeField;
    ClientDataSetTime: TDateTimeField;
    ClientDataSetPaymentType: TStringField;
    ClientDataSetPaymentAmount: TCurrencyField;
    ClientDataSetDescription: TMemoField;
    ClientDataSetQuantity: TIntegerField;
    DBGrid1: TDBGrid;
    DataSource: TDataSource;
    StyledDBNavigator: TStyledDbNavigator;
    CustomImagesCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ToolButtonclick(Sender: TObject);
    procedure CreateButtonClick(Sender: TObject);
    procedure UpdateDbNav(Sender: TObject);
    procedure PopUpMenuClick(Sender: TObject);
    procedure cbChangeStyleSelect(Sender: TObject);
    procedure CustomImagesCheckBoxClick(Sender: TObject);
  private
    FDbNavigator: TDbNavigator;
    FStyledDbNavigator: TStyledDbNavigator;
    procedure CreateStyledDbNavigator;
    procedure CreateDbNavigators;
    procedure CreateDbNavigator;
    procedure BuildStyleList;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

uses
  Themes,
  Vcl.StyledButtonEditorUnit;

procedure TfmMain.CreateButtonClick(Sender: TObject);
begin
  CreateDbNavigators;
  LeftPanel.Visible := False;
  BottomClientPanel.Visible := True;
end;

procedure TfmMain.CreateStyledDbNavigator;
begin
  FStyledDbNavigator := TStyledDbNavigator.Create(Self);
  //FStyledDbNavigator.Flat := True;
  FStyledDbNavigator.ShowCaptions := SHOW_CAPTIONS;
  FStyledDbNavigator.Align := DBNAV_ALIGN;
  FStyledDbNavigator.Kind := DBNAV_KIND;
  FStyledDbNavigator.Height := DBNAV_HEIGHT;
  FStyledDbNavigator.Width := DBNAV_WIDTH;
  FStyledDbNavigator.StyleElements := [];
  FStyledDbNavigator.VisibleButtons := [nbFirst, nbPrior, nbNext, nbLast];
  FStyledDbNavigator.DataSource := DataSource;
  FStyledDbNavigator.Parent := Self;
end;

procedure TfmMain.CustomImagesCheckBoxClick(Sender: TObject);
begin
  if StyledDBNavigator.Images = ImageList32 then
  begin
    FStyledDBNavigator.Images := nil;
    StyledDBNavigator.Images := nil;
  end
  else
  begin
    FStyledDBNavigator.Images := ImageList32;
    StyledDBNavigator.Images := ImageList32;
  end;
end;

procedure TfmMain.CreateDbNavigator;
begin
  FDbNavigator := TDbNavigator.Create(Self);
  //FDbNavigator.Flat := True;
  FDbNavigator.Align := DBNAV_ALIGN;
  FDbNavigator.Kind := DBNAV_KIND;
  FDbNavigator.Height := DBNAV_HEIGHT;
  FDbNavigator.Width := DBNAV_WIDTH;
  FDbNavigator.StyleElements := [];
  FDbNavigator.VisibleButtons := [nbFirst, nbPrior, nbNext, nbLast];
  FDbNavigator.DataSource := DataSource;
  FDbNavigator.Parent := Self;
end;

procedure TfmMain.CreateDbNavigators;
begin
  //Create Standard DbNavigator
  CreateDbNavigator;

  //Create Styled DbNavigator
  CreateStyledDbNavigator;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  BuildStyleList;
  Caption := Application.Title;
  FlatCheckBox.Checked := FLAT_BUTTONS;
  ShowCaptionsCheckBox.Checked := SHOW_CAPTIONS;
  tbWidth.Position := DBNAV_WIDTH;
  tbHeight.Position := DBNAV_HEIGHT;
end;

procedure TfmMain.PopUpMenuClick(Sender: TObject);
begin
  ShowMessage((Sender as TMenuItem).Caption);
end;

procedure TfmMain.ToolButtonclick(Sender: TObject);
begin
  if Sender is TToolButton then
    ShowMessage(TToolButton(Sender).Caption)
  else if Sender is TStyledNavButton then
    EditStyledButton(TStyledNavButton(Sender));
end;

procedure TfmMain.UpdateDbNav(Sender: TObject);
begin
  if Assigned(FStyledDbNavigator) then
  begin
    FStyledDbNavigator.Flat := FlatCheckBox.Checked;
    FStyledDbNavigator.ShowCaptions := ShowCaptionsCheckBox.Checked;
    FStyledDbNavigator.Height := tbHeight.Position;
    FStyledDbNavigator.Width := tbWidth.Position;
  end;
  if Assigned(FDbNavigator) then
  begin
    FDbNavigator.Flat := FlatCheckBox.Checked;
    FDbNavigator.Width := tbWidth.Position;
    FDbNavigator.Height := tbHeight.Position;
  end;
  DbNavigator.Flat := FlatCheckBox.Checked;
  DbNavigator.Width := tbWidth.Position;
  DbNavigator.Height := tbHeight.Position;
  StyledDbNavigator.Flat := FlatCheckBox.Checked;
  StyledDbNavigator.ShowCaptions := ShowCaptionsCheckBox.Checked;
  StyledDbNavigator.Height := tbHeight.Position;
  StyledDbNavigator.Width := tbWidth.Position;
end;

procedure TfmMain.BuildStyleList;
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

procedure TfmMain.cbChangeStyleSelect(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    TStyleManager.TrySetStyle(cbChangeStyle.Text);
  finally
    Screen.Cursor := crDefault;
  end;
end;

initialization
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

end.
