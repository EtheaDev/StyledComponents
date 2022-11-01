{******************************************************************************}
{                                                                              }
{       StyledTaskDialog: a Task Dialog Component with StyleButtons            }
{                                                                              }
{       Copyright (c) 2022 (Ethea S.r.l.)                                      }
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
unit Vcl.StyledTaskDialog;

{$INCLUDE StyledComponents.inc}

interface

uses
  System.SysUtils
  , System.Classes
  , WinApi.Windows
  , Vcl.Dialogs
  , Vcl.Graphics
  , Vcl.Forms
  , Vcl.StyledButton
  ;

type
  TStyledDialogIcons = array[TMsgDlgType] of TIcon;

  //  Abstraction of an SVG document
  ITaskDialogLauncher = interface
    ['{B2F16F98-C163-4706-A803-E624126D8DF6}']
    function DoExecute(ParentWnd: HWND;
      const ADialogType: TMsgDlgType;
      const ATaskDialog: TCustomTaskDialog): boolean;
  end;

{$WARN SYMBOL_PLATFORM OFF}
{ TaskDialog based message dialog; requires Windows Vista or later }
type
  TStyledTaskDialog = class(TTaskDialog)
  private
    FHelpFile: string;
    FParentWnd: HWND;
    FPosition: TPoint;
  strict protected
    function DoExecute(ParentWnd: HWND): Boolean; override;
    procedure DoOnButtonClicked(AModalResult: Integer; var CanClose: Boolean); override;
    procedure DoOnDialogCreated; override;
    procedure DoOnHelp; override;
    procedure DoOnHyperlinkClicked(const AURL: string); override;
  public
    function Execute(ParentWnd: HWND): Boolean; overload; override;
    property HelpFile: string read FHelpFile write FHelpFile;
    property Position: TPoint read FPosition write FPosition;
  end;

function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1; CustomIcon : TICon = nil): Integer; overload;
function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1; CustomIcon : TICon = nil): Integer; overload;
function StyledTaskDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1; CustomIcon : TIcon = nil): Integer; overload;
function StyledTaskDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1; CustomIcon : TIcon = nil): Integer; overload;

procedure SetUseAlwaysTaskDialog(Value: boolean);
procedure RegisterCustomExecute(const AShowStyledTaskDialog: ITaskDialogLauncher);
procedure UnregisterCustomExecute;
procedure InitializeDialogs(AFont: TFont; AUseTaskDialog: Boolean);
procedure RegisterCustomIcons(const ACustomIcons: TStyledDialogIcons);
procedure UnregisterCustomIcons;
function GetTaskDlgType(const AIcon: TTaskDialogIcon): TMsgDlgType;

implementation

uses
  System.TypInfo
  , System.Math
  , System.Types
  , Vcl.Themes
  , Winapi.CommCtrl
  , System.WideStrUtils
  , Winapi.MultiMon
  , System.HelpIntfs
  , Vcl.Controls
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  , Vcl.Consts
  , Winapi.ShellApi
  , Vcl.StyledCmpMessages
  , Vcl.StyledCmpStrUtils;


var
  TaskDialogExecute: ITaskDialogLauncher;
  CustomIcons: TStyledDialogIcons;
  DialogFont: TFont;
  UseAlwaysTaskDialog: boolean;

  ButtonNames: array[TMsgDlgBtn] of string = (
    'Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
    'YesToAll','Help','Close');
  ModalResults: array[TMsgDlgBtn] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0, mrClose);

function IsTaskMessageSupported : Boolean;
begin
{$IFDEF D16+}
  Result := (Win32MajorVersion >= 6) and StyleServices.Enabled;
{$ELSE}
  Result := (Win32MajorVersion >= 6) and StyleServices.Enabled;
{$ENDIF}
end;

function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer = -1; Y: Integer = -1; CustomIcon : TICon = nil): Integer;
var
  DefaultButton: TMsgDlgBtn;
begin
  if      mbYes       in Buttons then DefaultButton := mbYes
  else if mbOK        in Buttons then DefaultButton := mbOK
  else if mbNo        in Buttons then DefaultButton := mbNo
  else if mbCancel    in Buttons then DefaultButton := mbCancel
  else if mbAbort     in Buttons then DefaultButton := mbAbort
  else if mbRetry     in Buttons then DefaultButton := mbRetry
  else if mbIgnore    in Buttons then DefaultButton := mbIgnore
  else if mbAll       in Buttons then DefaultButton := mbAll
  else if mbNoToAll   in Buttons then DefaultButton := mbNoToAll
  else if mbYesToAll  in Buttons then DefaultButton := mbYesToAll
  else if mbHelp      in Buttons then DefaultButton := mbHelp
  else if mbClose     in Buttons then DefaultButton := mbClose
  else DefaultButton := mbYes;
  if Buttons = [] then
    Buttons := [mbOK];
  Result := StyledMessageDlgPos(Msg, DlgType, Buttons, DefaultButton, HelpCtx, X, Y, CustomIcon);
end;

function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint; X: Integer = -1; Y: Integer = -1;
  CustomIcon : TICon = nil): Integer;
var
  Dlg : TForm;
  MyMsg : string;
  LIcon: TIcon;
  LImage: TImage;

  procedure ChangeButtonCaption(MsgDlgBtn : TMsgDlgBtn;
    const Caption : string);
  var
    Button : TButton;
  begin
    Button := Dlg.FindComponent(ButtonNames[MsgDlgBtn]) as TButton;
    if Assigned(Button) then
      Button.Caption := Caption;
  end;

begin
  if IsTaskMessageSupported and UseAlwaysTaskDialog then
  begin
    //Use a TaskDialog to Show the message instead of a MessageDialog
    Result := StyledTaskDlgPos('',Msg,DlgType,Buttons,DefaultButton,HelpCtx,X,Y,CustomIcon);
  end
  else
  begin
    //Use standard MessageDlg
    //clear message for Hyperlinks (becasue are not supported)
    MyMsg := ClearHRefs(Msg);
    Dlg := CreateMessageDialog(MyMsg, DlgType, Buttons, DefaultButton);
    try
      if Assigned(DialogFont) then
        Dlg.Font.Assign(DialogFont);

      Dlg.HelpContext := HelpCtx;
      if X >= 0 then Dlg.Left := X;
      if Y >= 0 then Dlg.Top := Y;
      if (Y < 0) and (X < 0) then Dlg.Position := poScreenCenter;

      ChangeButtonCaption(mbYes,'&'+STR_YES);
      ChangeButtonCaption(mbNo,'&'+STR_NO);
      ChangeButtonCaption(mbOK,STR_OK);
      ChangeButtonCaption(mbCancel,STR_CANCEL);
      ChangeButtonCaption(mbAbort,STR_ABORT);
      ChangeButtonCaption(mbRetry,STR_RETRY);
      ChangeButtonCaption(mbIgnore,STR_IGNORE);
      ChangeButtonCaption(mbAll,'&'+STR_ALL);
      ChangeButtonCaption(mbNoToAll,'&'+STR_NOTOALL);
      ChangeButtonCaption(mbYesToAll,'&'+STR_YESTOALL);
      ChangeButtonCaption(mbHelp,'&'+STR_HELP);
      ChangeButtonCaption(mbClose,'&'+STR_CLOSE);

      case DlgType of
        mtWarning      : Dlg.Caption := STR_WARNING;
        mtError        : Dlg.Caption := STR_ERROR;
        mtInformation  : Dlg.Caption := STR_INFORMATION;
        mtConfirmation : Dlg.Caption := STR_CONFIRM;
        mtCustom       : Dlg.Caption := STR_INFORMATION;
      end;

      if Assigned(CustomIcon) then
        LIcon := CustomIcon
      else if Assigned(CustomIcons[DlgType]) then
        LIcon := CustomIcons[DlgType]
      else
        LIcon := nil;

      if Assigned(LIcon) then
      begin
        LImage := Dlg.FindComponent('Image') as TImage;
        if Assigned(LImage) then
          LImage.Picture.Assign(LIcon);
      end;

      Result := Dlg.ShowModal;
    finally
      Dlg.Free;
    end;
  end;
end;

const
  tdbHelp = -1;

procedure InitializeDialogs(AFont: TFont; AUseTaskDialog: Boolean);
begin
  if Assigned(AFont) then
  begin
    if not Assigned(DialogFont) then
      DialogFont := TFont.Create;
    DialogFont.Assign(AFont);
    UseAlwaysTaskDialog := AUseTaskDialog;
  end
  else
    FreeAndNil(DialogFont);
end;

procedure UnregisterCustomIcons;
begin
  CustomIcons[mtWarning] := nil;
  CustomIcons[mtError] := nil;
  CustomIcons[mtInformation] := nil;
  CustomIcons[mtConfirmation] := nil;
  CustomIcons[mtCustom] := nil;
end;

procedure RegisterCustomIcons(const ACustomIcons: TStyledDialogIcons);
begin
  UnregisterCustomIcons;
  CustomIcons := ACustomIcons;
end;

procedure SetUseAlwaysTaskDialog(Value: boolean);
begin
  UseAlwaysTaskDialog := Value;
end;

procedure RegisterCustomExecute(
  const AShowStyledTaskDialog: ITaskDialogLauncher);
begin
  TaskDialogExecute := AShowStyledTaskDialog;
end;

procedure UnRegisterCustomExecute;
begin
  TaskDialogExecute := nil;
end;

function GetTaskDlgType(
  const AIcon: TTaskDialogIcon): TMsgDlgType;
begin
  if AIcon = tdiNone then
    Result := TMsgDlgType.mtCustom
  else if AIcon = tdiWarning then
    Result := TMsgDlgType.mtWarning
  else if AIcon = tdiError then
    Result := TMsgDlgType.mtError
  else if AIcon = tdiInformation then
    Result := TMsgDlgType.mtInformation
  else if AIcon = tdiShield then
    Result := TMsgDlgType.mtCustom
  else
    Result := TMsgDlgType.mtInformation;
end;

function DoTaskMessageDlgPos(const Instruction, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  X: Integer = -1; Y: Integer = -1;
  CustomIcon : TIcon = nil): Integer;
const
  IconMap: array[TMsgDlgType] of TTaskDialogIcon = (tdiWarning, tdiError,
    tdiInformation, tdiInformation, tdiNone);
  LModalResults: array[TMsgDlgBtn] of Integer = (mrYes, mrNo, mrOk, mrCancel,
    mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll, mrYesToAll, tdbHelp, mrClose);
var
  DlgBtn: TMsgDlgBtn;
  LTaskDialog: TStyledTaskDialog;
  LTaskDialogButtonItem: TTaskDialogBaseButtonItem;
begin
  Application.ModalStarted;
  LTaskDialog := TStyledTaskDialog.Create(nil);
  try
    // Assign buttons
    for DlgBtn := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    begin
      if DlgBtn in Buttons then
      begin
        LTaskDialogButtonItem := LTaskDialog.Buttons.Add;
        //Button Caption translated
        case DlgBtn of
          mbYes: LTaskDialogButtonItem.Caption := '&'+STR_YES;
          mbNo: LTaskDialogButtonItem.Caption := '&'+STR_NO;
          mbOK: LTaskDialogButtonItem.Caption := STR_OK;
          mbCancel: LTaskDialogButtonItem.Caption := STR_CANCEL;
          mbAbort: LTaskDialogButtonItem.Caption := STR_ABORT;
          mbRetry: LTaskDialogButtonItem.Caption := STR_RETRY;
          mbIgnore: LTaskDialogButtonItem.Caption := STR_IGNORE;
          mbAll: LTaskDialogButtonItem.Caption := '&'+STR_ALL;
          mbNoToAll: LTaskDialogButtonItem.Caption := '&'+STR_NOTOALL;
          mbYesToAll: LTaskDialogButtonItem.Caption := '&'+STR_YESTOALL;
          mbHelp: LTaskDialogButtonItem.Caption := '&'+STR_HELP;
          mbClose: LTaskDialogButtonItem.Caption := '&'+STR_CLOSE;
        end;
        if DlgBtn = DefaultButton then
          LTaskDialogButtonItem.Default := True;
        LTaskDialogButtonItem.ModalResult := LModalResults[DlgBtn];
      end;
    end;

    // Set dialog properties
    with LTaskDialog do
    begin
      CommonButtons := [];
      if Application.UseRightToLeftReading then
        Flags := Flags + [tfRtlLayout];
      if pos('<A HREF=',Msg) > 0 then
        Flags := Flags + [tfEnableHyperlinks];

      HelpContext := HelpCtx;
      MainIcon :=  IconMap[DlgType];
      Position := Point(X, Y);
      Text := Msg;
      //Caption translated
      case DlgType of
        mtWarning      : LTaskDialog.Caption := STR_WARNING;
        mtError        : LTaskDialog.Caption := STR_ERROR;
        mtInformation  : LTaskDialog.Caption := STR_INFORMATION;
        mtConfirmation : LTaskDialog.Caption := STR_CONFIRM;
        mtCustom       : LTaskDialog.Caption := STR_INFORMATION;
      end;

      if Instruction <> '' then
        Title := Instruction
      else
        Title :=  LTaskDialog.Caption;

      if Assigned(CustomIcon) then
      begin
        Flags := Flags + [tfUseHiconMain];
        LTaskDialog.CustomMainIcon.Assign(CustomIcon);
      end;
    end;

    // Show dialog and return result
    Result := mrNone;
    if LTaskDialog.Execute then
      Result := LTaskDialog.ModalResult;
  finally
    LTaskDialog.Free;
    Application.ModalFinished;
  end;
end;

function StyledTaskDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer = -1; Y: Integer = -1; CustomIcon : TIcon = nil): Integer;
var
  DefaultButton: TMsgDlgBtn;
begin
  if      mbYes       in Buttons then DefaultButton := mbYes
  else if mbOK        in Buttons then DefaultButton := mbOK
  else if mbNo        in Buttons then DefaultButton := mbNo
  else if mbCancel    in Buttons then DefaultButton := mbCancel
  else if mbAbort     in Buttons then DefaultButton := mbAbort
  else if mbRetry     in Buttons then DefaultButton := mbRetry
  else if mbIgnore    in Buttons then DefaultButton := mbIgnore
  else if mbAll       in Buttons then DefaultButton := mbAll
  else if mbNoToAll   in Buttons then DefaultButton := mbNoToAll
  else if mbYesToAll  in Buttons then DefaultButton := mbYesToAll
  else if mbHelp      in Buttons then DefaultButton := mbHelp
  else if mbClose     in Buttons then DefaultButton := mbClose

  else DefaultButton := mbYes;

  Result := StyledTaskDlgPos(Title, Msg, DlgType, Buttons, DefaultButton, HelpCtx, X, Y, CustomIcon);
end;

function StyledTaskDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1; CustomIcon : TIcon = nil): Integer;
var
  MsgWithTitle: string;
begin
  if Title <> '' then
    MsgWithTitle := UpperCase(Title)+sLineBreak+Msg
  else
    MsgWithTitle := Msg;
  if IsTaskMessageSupported then
    Result := DoTaskMessageDlgPos(Title, Msg, DlgType, Buttons, HelpCtx, DefaultButton, X, Y, CustomIcon)
  else
    Result := StyledMessageDlgPos(MsgWithTitle, DlgType, Buttons, DefaultButton, HelpCtx, -1, -1, CustomIcon);
end;

{ TStyledTaskDialog }
function TStyledTaskDialog.DoExecute(ParentWnd: HWND): Boolean;
type
  TTaskDialogIcon = (tdiWarning, tdiError,
    tdiInformation, tdiShield, tdiNone);
const
  IconMap: array[TTaskDialogIcon] of TMsgDlgType =
    (TMsgDlgType.mtWarning,
     TMsgDlgType.mtError,
     TMsgDlgType.mtInformation,
     TMsgDlgType.mtCustom,
     TMsgDlgType.mtConfirmation);
var
  LTaskDlgType: TMsgDlgType;
begin
  LTaskDlgType := GetTaskDlgType(MainIcon);
  //Use a custom interface if registered
  if Assigned(TaskDialogExecute) then
    Result := TaskDialogExecute.DoExecute(ParentWnd,
      LTaskDlgType, Self)
  else
    Result := inherited DoExecute(ParentWnd);
end;

procedure TStyledTaskDialog.DoOnButtonClicked(AModalResult: Integer;
  var CanClose: Boolean);
begin
  if AModalResult = tdbHelp then
  begin
    CanClose := False;
    DoOnHelp;
  end;
end;

procedure TStyledTaskDialog.DoOnDialogCreated;
var
  Rect: TRect;
  LX, LY: Integer;
  LHandle: HMONITOR;
  LMonitorInfo: TMonitorInfo;
begin
  LX := Position.X;
  LY := Position.Y;
  LHandle := MonitorFromWindow(FParentWnd, MONITOR_DEFAULTTONEAREST);
  LMonitorInfo.cbSize := SizeOf(LMonitorInfo);
  if GetMonitorInfo(LHandle, {$IFNDEF CLR}@{$ENDIF}LMonitorInfo) then
    with LMonitorInfo do
    begin
      GetWindowRect(Handle, Rect);
      if LX < 0 then
        LX := ((rcWork.Right - rcWork.Left) - (Rect.Right - Rect.Left)) div 2;
      if LY < 0 then
        LY := ((rcWork.Bottom - rcWork.Top) - (Rect.Bottom - Rect.Top)) div 2;
      Inc(LX, rcWork.Left);
      Inc(LY, rcWork.Top);
      SetWindowPos(Handle, 0, LX, LY, 0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
    end;
end;

procedure TStyledTaskDialog.DoOnHelp;
var
  LHelpFile: string;
  LHelpSystem: IHelpSystem;

{$IFNDEF D12+}
  procedure ShowHelpException(E: Exception);
  var
    Msg: string;
    Flags: Integer;
  begin
    Flags := MB_OK or MB_ICONSTOP;
    if Application.UseRightToLeftReading then
      Flags := Flags or MB_RTLREADING;
    Msg := E.Message;
    if (Msg <> '') and (AnsiLastChar(Msg) > '.') then
      Msg := Msg + '.';
    MessageBox(Application.Handle, PChar(Msg), PChar(Application.Title), Flags);
  end;
{$ENDIF}

begin
  if HelpContext <> 0 then
  begin
    if FHelpFile = '' then
      LHelpFile := Application.HelpFile
    else
      LHelpFile := HelpFile;
    if System.HelpIntfs.GetHelpSystem(LHelpSystem) then
    try
      LHelpSystem.Hook(Application.Handle, LHelpFile, HELP_CONTEXT, HelpContext);
    except
      on E: Exception do
        ShowHelpException(E);
    end;
  end;
end;

procedure TStyledTaskDialog.DoOnHyperlinkClicked(const AURL: string);
begin
  inherited;
  ShellExecute( Application.Handle, '' , PChar(AURL), nil,
    PChar(ExtractFilePath(AURL)), SW_SHOWNORMAL );
end;

function TStyledTaskDialog.Execute(ParentWnd: HWND): Boolean;
begin
  FParentWnd := ParentWnd;
  Result := inherited Execute(ParentWnd);
end;

initialization
  UseAlwaysTaskDialog := True;
  DialogFont := nil;

finalization
  if Assigned(DialogFont) then
    DialogFont.Free;

end.
