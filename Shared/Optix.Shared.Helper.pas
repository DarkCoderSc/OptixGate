{******************************************************************************}
{                                                                              }
{         ____             _     ____          _           ____                }
{        |  _ \  __ _ _ __| | __/ ___|___   __| | ___ _ __/ ___|  ___          }
{        | | | |/ _` | '__| |/ / |   / _ \ / _` |/ _ \ '__\___ \ / __|         }
{        | |_| | (_| | |  |   <| |__| (_) | (_| |  __/ |   ___) | (__          }
{        |____/ \__,_|_|  |_|\_\\____\___/ \__,_|\___|_|  |____/ \___|         }
{                             Project: Optix Gate                              }
{                                                                              }
{                                                                              }
{                   Author: DarkCoderSc (Jean-Pierre LESUEUR)                  }
{                   https://www.twitter.com/darkcodersc                        }
{                   https://bsky.app/profile/darkcodersc.bsky.social           }
{                   https://github.com/darkcodersc                             }
{                   License: GPL v3                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{  Disclaimer:                                                                 }
{  -----------                                                                 }
{    We are doing our best to prepare the content of this app and/or code.     }
{    However, The author cannot warranty the expressions and suggestions       }
{    of the contents, as well as its accuracy. In addition, to the extent      }
{    permitted by the law, author shall not be responsible for any losses      }
{    and/or damages due to the usage of the information on our app and/or      }
{    code.                                                                     }
{                                                                              }
{    By using our app and/or code, you hereby consent to our disclaimer        }
{    and agree to its terms.                                                   }
{                                                                              }
{    Any links contained in our app may lead to external sites are provided    }
{    for convenience only.                                                     }
{    Any information or statements that appeared in these sites or app or      }
{    files are not sponsored, endorsed, or otherwise approved by the author.   }
{    For these external sites, the author cannot be held liable for the        }
{    availability of, or the content located on or through it.                 }
{    Plus, any losses or damages occurred from using these contents or the     }
{    internet generally.                                                       }
{                                                                              }
{                                                                              }
{  Authorship (No AI):                                                         }
{  -------------------                                                         }
{   All code contained in this unit was written and developed by the author    }
{   without the assistance of artificial intelligence systems, large language  }
{   models (LLMs), or automated code generation tools. Any external libraries  }
{   or frameworks used comply with their respective licenses.	                 }
{                                                                              }
{******************************************************************************}

unit Optix.Shared.Helper;

interface

// ---------------------------------------------------------------------------------------------------------------------
uses
  System.Classes, System.SysUtils, System.Math,

  Generics.Collections,

  Winapi.Windows;
// ---------------------------------------------------------------------------------------------------------------------

type
  TContentFormater = class
  public
    type
      TStringKind  = (skAnsi, skUnicode);
      TStringKinds = set of TStringKind;

      TStringInformation = record
        Offset : Pointer;
        Size   : UInt64;  // In Bytes
        Kind   : TStringKind;

        {@M}
        function ToString() : String;
        function Length() : UInt64;
      end;
  public
    class function ProbeForStrings(const pBuffer : Pointer;
      const ABufferSize : UInt64; const AMinLength : Cardinal = 0) : TList<TStringInformation>; static;
    class function ExtractStrings(const pBuffer : Pointer; const ABufferSize : UInt64;
      const AMinLength : Cardinal = 0; AStringKinds : TStringKinds = []) : String; static;
    class function OutputPrintableChar(const AByte : Byte) : Char; overload; static;
    class function OutputPrintableChar(const pBuffer : Pointer; const ABufferSize : UInt64) : String; overload; static;
    class function ToHexTable(const pBuffer : Pointer; const ABufferSize : UInt64;
      const AStartOffset : UInt64 = 0; const AColumnLength : Cardinal = 16) : String; static;
  end;

  TMemoryUtils = class
  public
    class procedure StringToMemory(const AValue : String; out pData : Pointer; out ADataSize : UInt64); static;
    class procedure DwordToMemory(const AValue : DWORD; out pData : Pointer; out ADataSize : UInt64); static;
    class procedure QwordToMemory(const AValue : UInt64; out pData : Pointer; out ADataSize : UInt64); static;

    class function MemoryMultiStringToString(const pBuffer : Pointer; const ABufferSize : UInt64) : String; static;
    class function MemoryToString(const pBuffer : Pointer; const ABufferSize : UInt64) : String; static;
  end;

implementation

(* TContentFormater *)

function TContentFormater.TStringInformation.ToString() : String;
begin
  if Kind = skUnicode then
    SetString(Result, PWideChar(Offset), Size div SizeOf(WideChar))
  else begin
    var ATemp : AnsiString;

    SetString(ATemp, PAnsiChar(Offset), Size div SizeOf(AnsiChar));

    ///
    Result := String(ATemp);
  end;
end;

function TContentFormater.TStringInformation.Length() : UInt64;
begin
  // TODO : Delphi CE >= 13 use Ternary
  if Kind = skUnicode then
    result := Size div SizeOf(WideChar)
  else
    result := Size;
end;

class function TContentFormater.ProbeForStrings(const pBuffer : Pointer; const ABufferSize : UInt64;
  const AMinLength : Cardinal = 0) : TList<TStringInformation>;

  var
    pCurByte, pLastByte : PByte;
    AStringInformation : TStringInformation;

  function IsPrintable(const AByte : Byte) : Boolean;
  begin
    Result := AByte in [9, 32..126];
  end;

  procedure RegisterStringInformation();
  begin
    if (AStringInformation.Offset <> nil) then begin
      AStringInformation.Size   := NativeUInt(pCurByte) - NativeUInt(AStringInformation.Offset);

      if (AMinLength = 0) or (AStringInformation.Size >= AMinLength)  then
        result.Add(AStringInformation);

      ///
      AStringInformation.Offset := nil;
      AStringInformation.Size   := 0;
    end;
  end;

begin
  result := TList<TStringInformation>.Create();
  ///

  if not Assigned(pBuffer) then
    Exit();

  pCurByte := pBuffer;
  pLastByte := pCurByte + ABufferSize;

  AStringInformation.Offset := nil;
  AStringInformation.Size   := 0;

  while pCurByte < pLastByte do begin
    // Unicode Strings -------------------------------------------------------------------------------------------------
    if ((AStringInformation.Offset = nil) or (AStringInformation.Kind = skUnicode)) and (pCurByte + 1 < pLastByte) and
       (IsPrintable(pCurByte^)) and (PWord(pCurByte)^ and $00FF = pCurByte^) and (PWord(pCurByte)^ shr 8 = 0) then begin

      if AStringInformation.Offset = nil then begin
        AStringInformation.Kind := skUnicode;
        AStringInformation.Offset := pCurByte;
      end;

      ///
      Inc(pCurByte, 2);
    // Ansi Strings ----------------------------------------------------------------------------------------------------
    end else
    if ((AStringInformation.Offset = nil) or (AStringInformation.Kind = skAnsi)) and IsPrintable(pCurByte^) then begin
      if AStringInformation.Offset = nil then begin
        AStringInformation.Kind := skAnsi;
        AStringInformation.Offset := pCurByte;
      end;

      ///
      Inc(pCurByte);
    end else begin
      RegisterStringInformation();

      ///
      Inc(pCurByte);
    end;
    // -----------------------------------------------------------------------------------------------------------------
  end;

  ///
  RegisterStringInformation();
end;

class function TContentFormater.ExtractStrings(const pBuffer : Pointer; const ABufferSize : UInt64;
  const AMinLength : Cardinal = 0; AStringKinds : TStringKinds = []) : String;
begin
  result := '';
  ///

  if AStringKinds = [] then
    AStringKinds := [skAnsi, skUnicode];

  var AStringInformations := ProbeForStrings(pBuffer, ABufferSize, AMinLength);
  try
    var ACharCount := UInt64(0);
    ///

    for var AStringInformation in AStringInformations do
      Inc(ACharCount, AStringInformation.Length);

    var AStringBuilder := TStringBuilder.Create(ACharCount);
    try
      for var AStringInformation in AStringInformations do begin
        // TODO: Delphi CE13 "not in"
        if not (AStringInformation.Kind in AStringKinds) then
          continue;
        ///

        AStringBuilder.AppendLine(AStringInformation.ToString);
      end;
    finally
      result := AStringBuilder.ToString();

      FreeAndNil(AStringBuilder);
    end;
  finally
    if Assigned(AStringInformations) then
      FreeAndNil(AStringInformations);
  end;
end;

class function TContentFormater.OutputPrintableChar(const AByte : Byte) : Char;
begin
  if AByte in [32..126] then
    result := Chr(AByte)
  else
    result := '.';
end;

class function TContentFormater.OutputPrintableChar(const pBuffer : Pointer; const ABufferSize : UInt64) : String;
begin
  SetLength(result, ABufferSize);
  ///

  for var I := 0 to ABufferSize -1 do
    result[I +1] := OutputPrintableChar(PByte(NativeUInt(pBuffer) + I)^);
end;

class function TContentFormater.ToHexTable(const pBuffer : Pointer; const ABufferSize : UInt64;
  const AStartOffset : UInt64 = 0; const AColumnLength : Cardinal = 16) : String;
begin
  result := '';
  ///

  if (ABufferSize = 0) or not Assigned(pBuffer) then
    Exit();

  var AOutputBuilder := TStringBuilder.Create(SizeOf(NativeUInt) + (AColumnLength * 3) + (AColumnLength -1) + 16);
  try
    var ARowHex   : array of String;
    var ARowAscii : array of String;
    ///

    SetLength(ARowHex, AColumnLength);
    SetLength(ARowAscii, AColumnLength);
    ///

    var ATotalBytesRead := UInt64(0);
    repeat
      var ABytesToRead := Min(ABufferSize - ATotalBytesRead, AColumnLength);
      ///

      for var I := 0 to ABytesToRead -1 do begin
        var ptrByte := PByte(NativeUInt(pBuffer) + ATotalBytesRead + I);
        ///

        ARowHex[I]   := IntToHex(ptrByte^);
        ARowAscii[I] := OutputPrintableChar(ptrByte^);
      end;

      if ABytesToRead < AColumnLength then
        for var I := ABytesToRead to AColumnLength -1 do begin
          ARowHex[I]   := '';
          ARowAscii[I] := '';
        end;

      AOutputBuilder.AppendFormat('%p %-' + IntToStr((AColumnLength * 2) + AColumnLength) + 's| %s%s', [
        Pointer(AStartOffset + ATotalBytesRead),
        String.Join(' ', ARowHex),
        String.Join('', ARowAscii),
        sLineBreak
      ]);

      ///
      Inc(ATotalBytesRead, ABytesToRead);
    until ATotalBytesRead = ABufferSize;

    ///
    result := AOutputBuilder.ToString;
  finally
    if Assigned(AOutputBuilder) then
      FreeAndNil(AOutputBuilder);
  end;
end;

(* TMemoryUtils *)

class procedure TMemoryUtils.StringToMemory(const AValue : String; out pData : Pointer; out ADataSize : UInt64);
begin
  ADataSize := Length(AValue) * SizeOf(WideChar);

  GetMem(pData, ADataSize);

  CopyMemory(pData, PWideChar(AValue), ADataSize);
end;

class procedure TMemoryUtils.DwordToMemory(const AValue : DWORD; out pData : Pointer; out ADataSize : UInt64);
begin
  ADataSize := SizeOf(DWORD);

  GetMem(pData, ADataSize);

  CopyMemory(pData, @AValue, ADataSize);
end;

class procedure TMemoryUtils.QwordToMemory(const AValue : UInt64; out pData : Pointer; out ADataSize : UInt64);
begin
  ADataSize := SizeOf(UInt64);

  GetMem(pData, ADataSize);

  CopyMemory(pData, @AValue, ADataSize);
end;

class function TMemoryUtils.MemoryMultiStringToString(const pBuffer : Pointer; const ABufferSize : UInt64) : String;
begin
  if not Assigned(pBuffer) or (ABufferSize < 2) then
    Exit('');
  ///

  var AStringList := TStringList.Create();
  try
    var p := PWideChar(pBuffer);
    var pEnd := PWideChar(NativeUInt(pBuffer) + ABufferSize);

    while p < pEnd do begin
      var pCandidate := p;

      while pCandidate < pEnd do begin
        if pCandidate^ = #0 then
          break;

        ///
        Inc(pCandidate);
      end;

      var ACandidate : String;
      var ACandidateLength := (NativeUInt(pCandidate) - NativeUInt(p)) div SizeOf(WideChar);

      SetString(ACandidate, p, ACandidateLength);

      AStringList.Add(ACandidate);

      if (pCandidate < pEnd) and (pCandidate^ = #0) then
        Inc(pCandidate);

      p := pCandidate;
    end;

    ///
    result := AStringList.Text;
  finally
    if Assigned(AStringList) then
      FreeAndNil(AStringList);
  end;
end;

class function TMemoryUtils.MemoryToString(const pBuffer : Pointer; const ABufferSize : UInt64) : String;
begin
  if not Assigned(pBuffer) or (ABufferSize = 0) then
    Exit('');

  ///
  SetString(result, PWideChar(pBuffer), ABufferSize div 2);
end;

end.
