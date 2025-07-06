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
{                   License: Apache License 2.0                                }
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
{                                                                              }
{******************************************************************************}

unit Optix.WinApiEx;

interface

uses Winapi.Windows;

type
  //----------------------------------------------------------------------------

  TProcessorArchitecture = (
    paUnknown,
    pa86_32,
    pa86_64,
    paARM,
    paARM64
  );

  TSidNameUse = (
    SidTypeUser,
    SidTypeGroup,
    SidTypeDomain,
    SidTypeAlias,
    SidTypeWellKnownGroup,
    SidTypeDeletedAccount,
    SidTypeInvalid,
    SidTypeUnknown,
    SidTypeComputer,
    SidTypeLabel,
    SidTypeLogonSession
  );

  STORAGE_BUS_TYPE = (
    BusTypeUnknown = 0,
    BusTypeScsi,
    BusTypeAtapi,
    BusTypeAta,
    BusType1394,
    BusTypeSsa,
    BusTypeFibre,
    BusTypeUsb,
    BusTypeRAID,
    BusTypeiScsi,
    BusTypeSas,
    BusTypeSata,
    BusTypeMaxReserved = $7F
  );
  TStorageBusType = STORAGE_BUS_TYPE;

  //----------------------------------------------------------------------------

  NET_API_STATUS = DWORD;

  UNICODE_STRING = record
    Length         : USHORT;
    MaximumLength  : USHORT;
    Buffer         : PWideChar;
  end;
  TUnicodeString = UNICODE_STRING;
  PUnicodeString = ^TUnicodeString;

  {$A8}
  WKSTA_INFO_100 = record
    wki100_platform_id  : DWORD;
    wki100_computername : LPWSTR;
    wki100_langroup     : LPWSTR;
    wki100_ver_major    : DWORD;
    wki100_ver_minor    : DWORD;
  end;
  TWkstaInfo100 = WKSTA_INFO_100;
  PWkstaInfo100 = ^TWkstaInfo100;
  {$A4}


  DOMAIN_CONTROLLER_INFO = record
    DomainControllerName        : LPWSTR;
    DomainControllerAddress     : LPWSTR;
    DomainControllerAddressType : ULONG;
    DomainGuid                  : TGUID;
    DomainName                  : LPWSTR;
    DnsForestName               : LPWSTR;
    Flags                       : ULONG;
    DcSiteName                  : LPWSTR;
    ClientSiteName              : LPWSTR;
  end;
  TDomainControllerInfo = DOMAIN_CONTROLLER_INFO;
  PDomainControllerInfo = ^TDomainControllerInfo;

  SYSTEM_PROCESS_INFORMATION = record
    NextEntryOffset              : ULONG;
    NumberOfThreads              : ULONG;
    WorkingSetPrivateSize        : LARGE_INTEGER;
    HardFaultCount               : ULONG;
    NumberOfThreadsHighWaterMark : ULONG;
    CycleTime                    : ULONGLONG;
    CreateTime                   : _FILETIME;
    UserTimer                    : LARGE_INTEGER;
    KernelTime                   : LARGE_INTEGER;
    ModuleName                   : TUnicodeString;
    BasePriority                 : LONG;
    ProcessID                    : NativeUInt;
    InheritedFromProcessId       : NativeUInt;
    HandleCount                  : ULONG;
    SessionId                    : ULONG;
    UniqueProcessKey             : ULONG_PTR;
    PeakVirtualSize              : ULONG_PTR;
    VirtualSize                  : ULONG_PTR;
    PageFaultCount               : ULONG;
    PeakWorkingSetSize           : ULONG_PTR;
    WorkingSetSize               : ULONG_PTR;
    QuotePeakPagedPoolUsage      : ULONG_PTR;
    QuotaPagedPoolUsage          : ULONG_PTR;
    QuotaPeakNonPagedPoolUsage   : ULONG_PTR;
    QuotaNonPagedPoolUsage       : ULONG_PTR;
    PagefileUsage                : ULONG_PTR;
    PeakPagefileUsage            : ULONG_PTR;
    PrivatePageCount             : ULONG_PTR;
    ReadOperationCount           : LARGE_INTEGER;
    WriteOperationCount          : LARGE_INTEGER;
    OtherOperationCount          : LARGE_INTEGER;
    ReadTransferCount            : LARGE_INTEGER;
    WriteTransferCount           : LARGE_INTEGER;
    OtherTransferCount           : LARGE_INTEGER
  end;
  TSystemProcessInformation = SYSTEM_PROCESS_INFORMATION;
  PSystemProcessInformation = ^TSystemProcessInformation;

  STORAGE_QUERY_TYPE = (
    PropertyStandardQuery = 0,
    PropertyExistsQuery,
    PropertyMaskQuery,
    PropertyQueryMaxDefined
  );
  TStorageQueryType = STORAGE_QUERY_TYPE;

  STORAGE_PROPERTY_ID = (
    StorageDeviceProperty = 0,
    StorageAdapterProperty
  );
  TStoragePropertyID = STORAGE_PROPERTY_ID;

  STORAGE_PROPERTY_QUERY = record
    PropertyId           : STORAGE_PROPERTY_ID;
    QueryType            : STORAGE_QUERY_TYPE;
    AdditionalParameters : array[0..9] of AnsiChar;
  end;
  TStoragePropertyQuery = STORAGE_PROPERTY_QUERY;

  STORAGE_DEVICE_DESCRIPTOR = record
    Version               : DWORD;
    Size                  : DWORD;
    DeviceType            : Byte;
    DeviceTypeModifier    : Byte;
    RemovableMedia        : Boolean;
    CommandQueueing       : Boolean;
    VendorIdOffset        : DWORD;
    ProductIdOffset       : DWORD;
    ProductRevisionOffset : DWORD;
    SerialNumberOffset    : DWORD;
    BusType               : STORAGE_BUS_TYPE;
    RawPropertiesLength   : DWORD;
    RawDeviceProperties   : array[0..0] of AnsiChar;
  end;
  TStorageDeviceDescriptor = STORAGE_DEVICE_DESCRIPTOR;
  PStorageDeviceDescriptor = ^TStorageDeviceDescriptor;

  STORAGE_DESCRIPTOR_HEADER = record
    Version : DWORD;
    Size    : DWORD;
  end;
  TStorageDescriptorHeader = STORAGE_DESCRIPTOR_HEADER;
  PStorageDescriptorHeader = ^TStorageDescriptorHeader;

//------------------------------------------------------------------------------

const
  NERR_Success = 0;

  DS_DIRECTORY_SERVICE_REQUIRED = $00000010;

  SECURITY_NT_AUTHORITY : TSIDIdentifierAuthority = (
    Value: (0, 0, 0, 0, 0, 5)
  );

  DOMAIN_ALIAS_RID_ADMINS       = $00000220;
  SECURITY_BUILTIN_DOMAIN_RID   = $00000020;

  SYSTEM_PROCESS_INFORMATION_CLASS  = 5;
  PROCESS_QUERY_LIMITED_INFORMATION = $00001000;

  PROCESSOR_ARCHITECTURE_ARM64 = 12;

//------------------------------------------------------------------------------

(* Netapi32.dll *)
function DsGetDcNameW(
  ComputerName         : LPCWSTR;
  DomainName           : LPCWSTR;
  GUID                 : PGUID;
  SiteName             : LPCWSTR;
  Flags                : ULONG;
  out DomainControllerInfo : PDomainControllerInfo
) : NET_API_STATUS; stdcall; external 'Netapi32.dll' Name 'DsGetDcNameW';

function NetWkstaGetInfo(
  servername : LPWSTR;
  level      : DWORD;
  out bufptr : Pointer
) : NET_API_STATUS; stdcall; external 'Netapi32.dll' Name 'NetWkstaGetInfo';

function NetApiBufferFree(
  Buffer : Pointer
) : NET_API_STATUS; stdcall; external 'Netapi32.dll';

(* Advapi32.dll *)

function CheckTokenMembership(
  TokenHandle  : THandle;
  SIdToCheck   : PSID;
  var IsMember : Boolean
): BOOL; stdcall; external 'Advapi32.dll';

(* NTDLL.dll *)

function NtQuerySystemInformation(
  SystemInformationClass  : DWORD;
  SystemInformation       : Pointer;
  SystemInformationLength : DWORD;
  var ReturnLength        : DWORD
) : Cardinal; stdcall; external 'NTDLL.DLL';

function QueryFullProcessImageNameW(
  hProcess   : THandle;
  dwFlags    : DWORD;
  lpExeName  : PWideChar;
  var dwSize : DWORD
): BOOL; stdcall; external kernel32 name 'QueryFullProcessImageNameW';

//------------------------------------------------------------------------------

function ProcessorArchitectureToString(const AValue : TProcessorArchitecture) : String;

implementation

{ _.ProcessorArchitectureToString }
function ProcessorArchitectureToString(const AValue : TProcessorArchitecture) : String;
begin
  case AValue of
    pa86_32   : result := 'x86 (32Bit)';
    pa86_64   : result := 'x86-64 / AMD64 (64Bit)';
    paARM     : result := 'ARM (32Bit)';
    paARM64   : result := 'ARM64 (64Bit)';
    else
      result := 'Unknown';
  end;
end;

end.
