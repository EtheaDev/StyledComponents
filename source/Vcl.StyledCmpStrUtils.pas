{******************************************************************************}
{                                                                              }
{       StyledCmpStrUtils: String utils for Styled Component                   }
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
unit Vcl.StyledCmpStrUtils;

{$INCLUDE StyledComponents.inc}

interface

function HRefToString(const HRef: string): string;
function StringToHRef(const LinkStr: string; DisplayLabel: string = ''): string;
function CopyByIndexes( const S : string; StartIndex, EndIndex : Integer ) : string;
function ExtractHrefValues(const HRef: string;
  out LinkStr, DisplayLabel: string) : boolean;
function ClearHRefs(const Msg: string; OnlyFileNotExists: boolean = False): string;
function GetErrorClassNameDesc(const ExceptionClassName : string;
  IsAccessViolation: boolean) : string;
function GetProjectURL: string;

implementation

uses
  System.SysUtils
  , Vcl.StyledCmpMessages
  ;

function GetProjectURL: string;
begin
  Result := 'https://github.com/EtheaDev/StyledComponents';
end;

function HRefToString(const HRef: string): string;
var
  DisplayLabel: string;
  LinkStr: string;
begin
  //Esempio: stringa in input: '<A HREF="c:\windows\system32\Notepad.exe'>Editor</A>'
  //risultato: Editor (c:\windows\system32\Notepad.exe)';

  if ExtractHrefValues(HRef, DisplayLabel, LinkStr) then
  begin
    //la stringa è coerente con la sintassi HREF: Calcolo DisplayLabel e LinkStr
    if not SameText(DisplayLabel, LinkStr) then
      Result := Format('%s (%s)',[LinkStr,DisplayLabel])
    else
      Result := LinkStr;
  end
  else
    Result := HRef;
end;

function StringToHRef(const LinkStr: string; DisplayLabel: string = ''): string;
var
  LDisplayLabel: string;
begin
  if DisplayLabel = '' then
  begin
    LDisplayLabel := ExtractFileName(LinkStr);
    if LDisplayLabel = '' then
      LDisplayLabel := LinkStr;
  end
  else
    LDisplayLabel := DisplayLabel;
  Result := Format('<A HREF="%s">%s</A>',[LinkStr, LDisplayLabel]);
end;

function ExtractHrefValues(const HRef: string;
  out LinkStr, DisplayLabel: string) : boolean;
var
  p1, p2, p3: integer;
begin
  //Esempio: stringa in input: '<A HREF="c:\windows\system32\Notepad.exe">Editor</A>'
  //DisplayLabel: Editor
  //LinkStr: c:\windows\system32\Notepad.exe';
  p1 := pos('>', HRef);
  p2 := Length(HRef)-3;
  p3 := pos('">',HRef);
  //controllo congruità
  if (p1 > 0) and (p3 > 0) and
    SameText(Copy(HRef,1,9),'<A HREF="') and
    SameText(Copy(HRef,p2,4),'</A>') then
  begin
    //la stringa è coerente con la sintassi HREF: Calcolo DisplayLabel e LinkStr
    DisplayLabel := Copy(HRef,p1+1,p2-p1-1);
    LinkStr := Copy(HRef,10,p1-11);
    Result := True;
  end
  else
  begin
    LinkStr := HRef;
    DisplayLabel := '';
    Result := False;
  end;
end;

function CopyByIndexes( const S : string; StartIndex, EndIndex : Integer ) : string;
begin
  Result := Copy( S, StartIndex, Succ(EndIndex-StartIndex) );
end;

function ClearHRefs(const Msg: string; OnlyFileNotExists: boolean = False): string;
var
  p1, p2: integer;
  SubMsg, HRef, LinkStr, DisplayLabel: string;
begin
  Result := '';
  SubMsg := Msg;
  while True do
  begin
    p1 := pos('<A HREF="', UpperCase(SubMsg));
    p2 := pos('</A>', UpperCase(SubMsg));
    if (p1 > 0) and (p2 > 0) then
    begin
      //Ho individuato un Hyperlink dalla posizione p1 alla p2+3
      HRef :=CopyByIndexes(SubMsg,p1,p2+3);
      ExtractHrefValues(HRef, LinkStr, DisplayLabel);
      if not OnlyFileNotExists or not FileExists(LinkStr) then
        Result := Result + Copy(SubMsg,1,p1-1)+HRefToString(HRef)
      else
        Result := Result + Copy(SubMsg,1,p1-1)+HRef;
      SubMsg := Copy(SubMsg,p2+4,maxint);
    end
    else
    begin
      Result := Result + SubMsg;
      break;
    end;
  end;
end;

function GetErrorClassNameDesc(const ExceptionClassName : string;
  IsAccessViolation: boolean) : string;
begin
  Result := '';
  if pos('Database', ExceptionClassName) > 0 then
    Result := EDATABASEERRORDESC
  else if IsAccessViolation then
    Result := EACCESSVIOLDESC
  else
    Result := EGENERICERROR;
end;

end.
