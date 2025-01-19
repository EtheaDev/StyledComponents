{******************************************************************************}
{                                                                              }
{  Main form used by two examples:                                             }
{  StyledTaskDialogDemo and AnimatedTaskDialogDemo                             }
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
unit StyledDialogDemoForm;

interface

uses
  System.SysUtils, Vcl.Forms, Vcl.CheckLst, Vcl.StyledTaskDialog, Vcl.ButtonStylesAttributes,
  Vcl.Samples.Spin, Vcl.Dialogs, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.Classes;

{$WARN SYMBOL_PLATFORM OFF}
type
  TfmStyledTaskDialog = class(TForm)
    TaskDialog: TTaskDialog;
    StyledTaskDialog: TStyledTaskDialog;
    ExtraGroupBox: TGroupBox;
    ExpandedTextLabel: TLabel;
    ExpandedTextMemo: TMemo;
    btUseStyledDialogComp: TButton;
    btUseNativeDialogComp: TButton;
    FooterTextLabel: TLabel;
    FooterTextMemo: TMemo;
    VerificationTextLabel: TLabel;
    VerificationTextMemo: TMemo;
    rgMainIcon: TRadioGroup;
    CaptionLabel: TLabel;
    CaptionEdit: TEdit;
    ClientPanel: TPanel;
    LeftPanelClient: TPanel;
    TitleLabel: TLabel;
    edTitle: TEdit;
    TextMessageLabel: TLabel;
    edMessage: TMemo;
    btStyledTaskDialog: TButton;
    btNativeTaskDialog: TButton;
    btRaiseDatabaseError: TButton;
    btStyledMsgDialog: TButton;
    btNativeMsgDialog: TButton;
    btRaiseGenericError: TButton;
    RightPanel: TPanel;
    DefaultButtonLabel: TLabel;
    rgDlgType: TRadioGroup;
    clbButtons: TCheckListBox;
    DefaultButtonComboBox: TComboBox;
    FamilyComboBox: TComboBox;
    DlgButtonTypeLabel: TLabel;
    gbResult: TGroupBox;
    MRLabel: TLabel;
    AlphaBlendSpinEdit: TSpinEdit;
    AlphaLabel: TLabel;
    cbUseCommandLinks: TCheckBox;
    cbUseTitleInMessageDlg: TCheckBox;
    btNativeShowMessage: TButton;
    btStyledShowMessage: TButton;
    ButtonsWidthSpinEdit: TSpinEdit;
    ButtonsWidthLabel: TLabel;
    lbAutoClickDelay: TLabel;
    AutoClickDelaySpinEdit: TSpinEdit;
    cbAutoClick: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ShowDlg(Sender: TObject);
    procedure RaiseError(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TaskDialogTimer(Sender: TObject; TickCount: Cardinal;
      var Reset: Boolean);
    procedure UseStyleDialogCompClick(Sender: TObject);
    procedure FontComboBoxSelect(Sender: TObject);
    procedure RaiseDatabaseError(Sender: TObject);
    procedure TaskDialogButtonClicked(Sender: TObject;
      ModalResult: TModalResult; var CanClose: Boolean);
    procedure TaskDialogDialogConstructed(Sender: TObject);
    procedure TaskDialogDialogCreated(Sender: TObject);
    procedure TaskDialogDialogDestroyed(Sender: TObject);
    procedure TaskDialogExpanded(Sender: TObject);
    procedure TaskDialogHyperlinkClicked(Sender: TObject);
    procedure TaskDialogNavigated(Sender: TObject);
    procedure TaskDialogRadioButtonClicked(Sender: TObject);
    procedure TaskDialogVerificationClicked(Sender: TObject);
    procedure InitializeDialogsClick(Sender: TObject);
  private
    FRadioButtonSelected: string;
    procedure ShowSelection(const AModalResult: TModalResult);
    procedure InitializeDialogs;
    function EnableAndFocusOKButton(const ATaskDialog: TCustomTaskDialog): Boolean;
  protected
    procedure Loaded; override;
  public
    procedure ShowError(Sender: TObject; E: Exception);
  end;

var
  fmStyledTaskDialog: TfmStyledTaskDialog;

implementation

{$R *.dfm}

uses
  System.TypInfo
  , System.UITypes
  , Winapi.ShellAPI
  , Winapi.Windows
  , Data.DB
  , Vcl.Graphics
  , Vcl.StyledButton
  , Vcl.StyledCmpMessages
  , Vcl.StyledCmpStrUtils
  , Vcl.StyledTaskDialogFormUnit;

procedure TfmStyledTaskDialog.InitializeDialogsClick(Sender: TObject);
begin
  InitializeDialogs;
end;

procedure TfmStyledTaskDialog.InitializeDialogs;
var
  LAutoClickDelay: Integer;
begin
  if cbAutoClick.Checked then
    LAutoClickDelay := AutoClickDelaySpinEdit.Value
  else
    LAutoClickDelay := -1;
  //Initialize Task Dialogs Defaults
  InitializeStyledTaskDialogs(cbUseTitleInMessageDlg.Checked,
    Screen.MessageFont,
    cbUseCommandLinks.Checked,
    FamilyComboBox.Text, AlphaBlendSpinEdit.Value,
    ButtonsWidthSpinEdit.Value,
    DEFAULT_STYLEDDIALOG_BUTTONSHEIGHT,
    LAutoClickDelay);
end;

procedure TfmStyledTaskDialog.FontComboBoxSelect(Sender: TObject);
begin
  InitializeDialogs;
end;

procedure TfmStyledTaskDialog.FormCreate(Sender: TObject);
var
  dt : TMsgDlgType;
  db : TMsgDlgBtn;
  Msg: string;
  LButtonName: string;
begin
  //Example to resize and change message font
  //Screen.MessageFont.Size := Round(Screen.MessageFont.Size * 1.2);
  //Screen.MessageFont.Name := 'Century Gothic';
  InitializeDialogs;

  ButtonsWidthSpinEdit.Value := DEFAULT_STYLEDDIALOG_BUTTONSWIDTH;
  AlphaBlendSpinEdit.Value := DEFAULT_STYLEDDIALOG_ALPHABLEND;

  Caption := Application.Title;
  MRLabel.Font.Style := [TFontStyle.fsBold];

  for dt := Low(TMsgDlgType) to High(TMsgDlgType)  do
    rgDlgType.Items.Add(GetEnumName(TypeInfo(TMsgDlgType), Ord(dt)));
  for db := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
  begin
    LButtonName := GetEnumName(TypeInfo(TMsgDlgBtn), Ord(db));
    clbButtons.Items.Add(LButtonName);
    if db = mbOK then
      clbButtons.Checked[Ord(db)] := True;
    DefaultButtonComboBox.Items.Add(LButtonName);
  end;

  rgDlgType.ItemIndex := 0;

  Msg :='The file was created: '+StringToHRef('C:\Windows\System32\license.rtf','license.rtf')+sLineBreak+
        'You can run: '+StringToHRef('C:\Windows\System32\Notepad.exe','Notepad Editor')+sLineBreak+
        'You can open folder: '+StringToHRef('C:\Windows\System32\')+sLineBreak+
        'You can visit site: '+StringToHRef('http://www.ethea.it','www.Ethea.it');

  edMessage.Text := Msg;
end;

procedure TfmStyledTaskDialog.ShowSelection(const AModalResult: TModalResult);
var
  LResultMsg: string;
begin
  case AModalResult of
    mrYes: LResultMsg := 'Yes';
    mrNo: LResultMsg := 'No';
    mrOk: LResultMsg := 'OK';
    mrCancel: LResultMsg := 'Cancel';
    mrAbort: LResultMsg := 'Abort';
    mrRetry: LResultMsg := 'Retry';
    mrIgnore: LResultMsg := 'Ignore';
    mrAll: LResultMsg := 'All';
    mrNoToAll: LResultMsg := 'NoToAll';
    mrYesToAll: LResultMsg := 'YesToAll';
    mrClose: LResultMsg := 'Close';
  else
    LResultMsg := '';
  end;
  if LResultMsg <> '' then
    MRLabel.Caption :=  Format('"%s"', [LResultMsg]);
end;

procedure TfmStyledTaskDialog.ShowDlg(Sender: TObject);
var
  LButtons: TMsgDlgButtons;
  db : TMsgDlgBtn;
  LDefaultButton: TMsgDlgBtn;
  LUseDefaultbutton: boolean;
  LResult: TModalResult;
  LDlgType: TMsgDlgType;
begin
  LButtons := [];
  LUseDefaultbutton := False;
  LDefaultButton := mbOK;
  LDlgType := TMsgDlgType(rgDlgType.ItemIndex);
  //HelpContext := 0;
  for db := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    if clbButtons.Checked[Ord(db)] then
    begin
      LButtons := LButtons + [db];
      if db = TMsgDlgBtn.mbHelp then
        HelpContext := 200;
      if SameText(DefaultButtonComboBox.Text, clbButtons.Items[Ord(db)]) then
      begin
        //The Default Button correponds to a selected Button
        LDefaultButton := db;
        LUseDefaultbutton := True;
      end;
    end;

  if LUseDefaultbutton then
  begin
    if Sender = btNativeShowMessage then
      //Call Delphi MessageDlg
      LResult := MessageDlg(edMessage.Text, LDlgType, LButtons, HelpContext, LDefaultButton)
    else if Sender = btStyledShowMessage then
      //Call Styled MessageDlg
      LResult := StyledMessageDlg(edMessage.Text, LDlgType, LButtons, HelpContext, LDefaultButton)
    else if Sender = btNativeTaskDialog then
      //Call Delphi TaskMessageDlg passing Default Button
      LResult := TaskMessageDlg(edTitle.Text, edMessage.Text, LDlgType, LButtons, HelpContext, LDefaultButton)
    else if Sender = btNativeMsgDialog then
      //Call Delphi MessageDlg (ignore Title!) passing Default Button
      LResult := MessageDlg(edMessage.Text, LDlgType, LButtons, 0, LDefaultButton)
    else if Sender = btStyledTaskDialog then
      //Call StyledTaskMessageDlg passing Default Button
      LResult := StyledTaskMessageDlg(edTitle.Text, edMessage.Text, LDlgType, LButtons, HelpContext, LDefaultButton)
    else if Sender = btStyledMsgDialog then
      //Call StyledMessageDlg (ignore Title!) passing Default Button
      LResult := StyledMessageDlg(edMessage.Text, LDlgType, LButtons, HelpContext, LDefaultButton)
    else
      LResult := mrNone;
  end
  else
  begin
    if Sender = btNativeShowMessage then
      //Call Delphi MessageDlg without Default Button
      LResult := MessageDlg(edMessage.Text, LDlgType, LButtons, HelpContext)
    else if Sender = btStyledShowMessage then
      //Call Styled MessageDlg without Default Button
      LResult := StyledMessageDlg(edMessage.Text, LDlgType, LButtons, HelpContext)
    else if Sender = btNativeTaskDialog then
      //Call Delphi TaskMessageDlg without Default Button
      LResult := TaskMessageDlg(edTitle.Text, edMessage.Text, LDlgType, LButtons, HelpContext)
    else if Sender = btNativeMsgDialog then
      //Call Delphi MessageDlg (ignore Title!) without Default Button
      LResult := MessageDlg(edMessage.Text, LDlgType, LButtons, 0)
    else if Sender = btStyledTaskDialog then
      //Call StyledTaskMessageDlg without Default Button
      LResult := StyledTaskMessageDlg(edTitle.Text, edMessage.Text, LDlgType, LButtons, HelpContext)
    else if Sender = btStyledMsgDialog then
      //Call StyledMessageDlgPos (ignore Title!) without Default Button
      LResult := StyledMessageDlg(edMessage.Text, LDlgType, LButtons, HelpContext)
    else
      LResult := mrNone;
  end;
  ShowSelection(LResult);
end;

procedure TfmStyledTaskDialog.FormDestroy(Sender: TObject);
begin
  ;
end;

procedure TfmStyledTaskDialog.Loaded;
begin
  //Font.Assign(Screen.IconFont);
  //Screen.MessageFont.Assign(Font);
  inherited;
end;



procedure TfmStyledTaskDialog.RaiseDatabaseError(Sender: TObject);
begin
  DatabaseError('Database Error!'+sLineBreak+
    'Please read the error file: '+
    StringToHRef('C:\Windows\System32\license.rtf','license.rtf'));
end;

procedure TfmStyledTaskDialog.RaiseError(Sender: TObject);
begin
  raise Exception.CreateHelp('Unexpected Error!'+sLineBreak+
    'Please read the error file: '+
    StringToHRef('C:\Windows\System32\license.rtf','license.rtf'),
      200);
end;

procedure TfmStyledTaskDialog.ShowError(Sender: TObject; E: Exception);
var
  Buttons: TMsgDlgButtons;
  Selection: Integer;
  LTitle: string;
  LMessage: string;
  LHelpContext: Integer;
  LUseTaskDialog: Boolean;
begin
  LTitle := GetErrorClassNameDesc(E.ClassName,
    E is EAccessViolation);
  LMessage := E.Message;
  LHelpContext := 0;

  LUseTaskDialog := (E.HelpContext = -100);

  if E.HelpContext <> 0 then
    LHelpContext := Abs(E.HelpContext);

  Buttons := [mbOK];
  if LHelpContext <> 0 then
    Buttons := Buttons + [mbHelp];
  if LUseTaskDialog and (Win32MajorVersion >= 6) then
  begin
    if E.InheritsFrom(EAccessViolation) then
      Buttons := Buttons + [mbAbort];

    Selection := StyledTaskMessageDlg(LTitle, LMessage, mtError, Buttons, LHelpContext, mbOK);
    if Selection = mrAbort then
      Application.Terminate
    else if Selection = -1 then
      Application.HelpContext(LHelpContext);
  end
  else
  begin
    Selection := StyledMessageDlg(LMessage, mtError, Buttons, LHelpContext, mbOK);
    if Selection = mrAbort then
      Application.Terminate
    else if Selection = -1 then
      Application.HelpContext(LHelpContext);
  end;
end;

procedure TfmStyledTaskDialog.UseStyleDialogCompClick(Sender: TObject);
const
  LModalResults: array[TMsgDlgBtn] of Integer = (mrYes, mrNo, mrOk, mrCancel,
    mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll, mrYesToAll, mrHelp, mrClose);

var
  LTaskDialog: TTaskDialog;
  LUseDefaultButton: Boolean;
  db: TMsgDlgBtn;
  LCustomButton: TTaskDialogButtonItem;

  procedure AddCommonButton(const AButton: TTaskDialogCommonButton);
  begin
    LTaskDialog.CommonButtons := LTaskDialog.CommonButtons + [AButton];
    if LUseDefaultButton then
      LTaskDialog.DefaultButton := AButton;
  end;

begin
  if Sender = btUseStyledDialogComp then
  begin
    //Styled TaskDialog
    LTaskDialog := StyledTaskDialog;
    StyledTaskDialog.AutoClick := cbAutoClick.Checked;
    StyledTaskDialog.AutoClickDelay := AutoClickDelaySpinEdit.Value;
    StyledTaskDialog.ButtonsWidth := ButtonsWidthSpinEdit.Value;
    StyledTaskDialog.Flags :=  StyledTaskDialog.Flags - [tfVerificationFlagChecked];
    {$IFDEF SKIA}
    StyledTaskDialog.UseAnimations := True;
    {$ENDIF}
  end
  else
  begin
    //Standard VCL TaskDialog
    LTaskDialog := TaskDialog;
  end;
  //Initialize TaskDialog proprerties
  LTaskDialog.Caption := CaptionEdit.Text;
  LTaskDialog.Title := edTitle.Text;
  LTaskDialog.Text := edMessage.Text;
  LTaskDialog.FooterText := FooterTextMemo.Text;
  LTaskDialog.ExpandedText := ExpandedTextMemo.Text;
  LTaskDialog.VerificationText := VerificationTextMemo.Text;
  LTaskDialog.MainIcon := rgMainIcon.ItemIndex;

  //Define TaskDialog Buttons (as CommonButtons and/or Custom Buttons)
  LTaskDialog.Buttons.Clear;
  LTaskDialog.CommonButtons := [];
  LTaskDialog.DefaultButton := tcbOk;
  LTaskDialog.Flags := LTaskDialog.Flags - [tfUseCommandLinks];
  for db := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
  begin
    if clbButtons.Checked[Ord(db)] then
    begin
      //For "Common Buttons" use CommonButtons property
      LUseDefaultButton := SameText(DefaultButtonComboBox.Text, clbButtons.Items[Ord(db)]);
      case db of
        TMsgDlgBtn.mbYes: AddCommonButton(tcbYes);
        TMsgDlgBtn.mbNo: AddCommonButton(tcbNo);
        TMsgDlgBtn.mbOK: AddCommonButton(tcbOK);
        TMsgDlgBtn.mbCancel: AddCommonButton(tcbCancel);
        TMsgDlgBtn.mbRetry: AddCommonButton(tcbRetry);
        TMsgDlgBtn.mbClose: AddCommonButton(tcbClose);
      else
        //For other buttons add "Custom button"
        LCustomButton := LTaskDialog.Buttons.add as TTaskDialogButtonItem;
        LCustomButton.Caption := Copy(GetEnumName(TypeInfo(TMsgDlgBtn), Ord(db)), 3, MaxInt);
        //Examples to use Command Links
        if cbUseCommandLinks.Checked then
        begin
          case db of
            TMsgDlgBtn.mbAbort: LCustomButton.CommandLinkHint := 'Abort operation and undo all changes...';
            TMsgDlgBtn.mbIgnore: LCustomButton.CommandLinkHint := 'Ignore operation and continue...';
            TMsgDlgBtn.mbAll: LCustomButton.CommandLinkHint := 'Include all items in operation...';
            TMsgDlgBtn.mbNoToAll: LCustomButton.CommandLinkHint := 'Discard operation to all items...';
            TMsgDlgBtn.mbYesToAll: LCustomButton.CommandLinkHint := 'Apply operation to all items...';
            TMsgDlgBtn.mbHelp: LCustomButton.CommandLinkHint := 'Open help guide...';
            TMsgDlgBtn.mbClose: LCustomButton.CommandLinkHint := 'Close operation...';
          end;
        end;
        LCustomButton.ModalResult := LModalResults[db];
        if LUseDefaultButton then
        begin
          LCustomButton.Default := True;
          LTaskDialog.DefaultButton := MsgDlgBtnToDefaultBtn(db);
        end;
        //If a "Custom button" was created then add flag tfUseCommandLinks if requested
        if cbUseCommandLinks.Checked then
          LTaskDialog.Flags := LTaskDialog.Flags + [tfUseCommandLinks];
      end;
    end;
  end;
  //Execute Dialog
  LTaskDialog.Execute(Self.Handle);
  //Show Result
  ShowSelection(LTaskDialog.ModalResult);
end;

procedure TfmStyledTaskDialog.TaskDialogButtonClicked(Sender: TObject;
  ModalResult: TModalResult; var CanClose: Boolean);
begin
  ;
end;

procedure TfmStyledTaskDialog.TaskDialogDialogConstructed(Sender: TObject);
begin
  ;
end;

procedure TfmStyledTaskDialog.TaskDialogDialogCreated(Sender: TObject);
begin
  EnableAndFocusOKButton(Sender as TCustomTaskDialog);
end;

procedure TfmStyledTaskDialog.TaskDialogDialogDestroyed(Sender: TObject);
begin
  ;
end;

procedure TfmStyledTaskDialog.TaskDialogExpanded(Sender: TObject);
begin
  ;
end;

procedure TfmStyledTaskDialog.TaskDialogHyperlinkClicked(Sender: TObject);
var
  LURL: String;
begin
  LURL := (Sender as TTaskDialog).URL;
  ShellExecute(Self.Handle, 'open' , PChar(LURL), nil, nil, SW_SHOW );
end;

procedure TfmStyledTaskDialog.TaskDialogNavigated(Sender: TObject);
begin
  ;
end;

procedure TfmStyledTaskDialog.TaskDialogRadioButtonClicked(Sender: TObject);
var
  LTaskDialog: TCustomTaskDialog;
begin
  if Sender is TCustomTaskDialog then
    LTaskDialog := TCustomTaskDialog(Sender)
  else
    LTaskDialog := nil;
  if Assigned(LTaskDialog) then
    FRadioButtonSelected := LTaskDialog.RadioButton.Caption;
end;

procedure TfmStyledTaskDialog.TaskDialogTimer(Sender: TObject; TickCount: Cardinal; var Reset: Boolean);
begin
   // TaskDialog1.ProgressBar.Position := MyThread.CurrentProgressPercent;
   // Demo
   //TaskDialog.ProgressBar.Position :=  TaskDialog.ProgressBar.Position + 1;
   TaskDialog.Execute(Self.Handle);
end;

function TfmStyledTaskDialog.EnableAndFocusOKButton(const ATaskDialog: TCustomTaskDialog): Boolean;
var
  LOKButton: TStyledButton;
begin
  //Example to enable/disable OK Button based on Verification Flag Checked
  //This is possible only with TStyledTaskDialog!
  Result := False;
  if ATaskDialog is TStyledTaskDialog then
  begin
    LOKButton := TStyledTaskDialog(ATaskDialog).FindDialogButton(mrOK);
    if Assigned(LOKButton) then
    begin
      LOKButton.Enabled := tfVerificationFlagChecked in ATaskDialog.Flags;
      if LOKButton.CanFocus then
        LOKButton.SetFocus;
      Result := True;
    end;
  end;
end;

procedure TfmStyledTaskDialog.TaskDialogVerificationClicked(Sender: TObject);
var
  LTaskDialog: TCustomTaskDialog;
begin
  LTaskDialog := Sender as TCustomTaskDialog;
  //In this examples, for StyledTaskDialog, clicking on Verification checkbox
  //enable/disable OK Button
  if not EnableAndFocusOKButton(LTaskDialog) then
  begin
    if tfVerificationFlagChecked in LTaskDialog.Flags then
      ShowMessage('Verification Click TRUE')
    else
      ShowMessage('Verification Click FALSE');
  end;
end;

end.

