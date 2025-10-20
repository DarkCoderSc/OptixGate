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
{                                                                              }
{******************************************************************************}

unit Optix.WinApiEx;

interface

uses Winapi.Windows;

type
  //--------------------------------------------------------------------------------------------------------------------

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

  TProcessInformationClass = (
    // ...
    ProcessBasicInformation = 0
    // ...
  );

  //--------------------------------------------------------------------------------------------------------------------

  NET_API_STATUS = DWORD;

  UNICODE_STRING = record
    Length         : USHORT;
    MaximumLength  : USHORT;
    Buffer         : PWideChar;
  end;
  TUnicodeString = UNICODE_STRING;
  PUnicodeString = ^TUnicodeString;

  WKSTA_INFO_100 = record
    wki100_platform_id  : DWORD;
    wki100_computername : LPWSTR;
    wki100_langroup     : LPWSTR;
    wki100_ver_major    : DWORD;
    wki100_ver_minor    : DWORD;
  end;
  TWkstaInfo100 = WKSTA_INFO_100;
  PWkstaInfo100 = ^TWkstaInfo100;

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
    WorkingSetPrivateSize        : ULONGLONG;
    HardFaultCount               : ULONG;
    NumberOfThreadsHighWaterMark : ULONG;
    CycleTime                    : ULONGLONG;
    CreateTime                   : _FILETIME;
    UserTimer                    : LARGE_INTEGER;
    KernelTime                   : LARGE_INTEGER;
    ModuleName                   : TUnicodeString;
    BasePriority                 : LONG;
    ProcessID                    : THandle;
    InheritedFromProcessId       : THandle;
    HandleCount                  : ULONG;
    SessionId                    : ULONG;
    UniqueProcessKey             : ULONG_PTR;
    PeakVirtualSize              : SIZE_T;
    VirtualSize                  : SIZE_T;
    PageFaultCount               : ULONG;
    PeakWorkingSetSize           : SIZE_T;
    WorkingSetSize               : SIZE_T;
    QuotePeakPagedPoolUsage      : SIZE_T;
    QuotaPagedPoolUsage          : SIZE_T;
    QuotaPeakNonPagedPoolUsage   : SIZE_T;
    QuotaNonPagedPoolUsage       : SIZE_T;
    PagefileUsage                : SIZE_T;
    PeakPagefileUsage            : SIZE_T;
    PrivatePageCount             : SIZE_T;
    ReadOperationCount           : LARGE_INTEGER;
    WriteOperationCount          : LARGE_INTEGER;
    OtherOperationCount          : LARGE_INTEGER;
    ReadTransferCount            : LARGE_INTEGER;
    WriteTransferCount           : LARGE_INTEGER;
    OtherTransferCount           : LARGE_INTEGER;
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

  _RTL_USER_PROCESS_PARAMETERS = record
    Reserved1     : array[0..16-1] of byte;
    Reserved2     : array[0..10-1] of PVOID;
    ImagePathName : TUnicodeString;
    CommandLine   : TUnicodeString;
  end;
  TRTLUserProcessParameters = _RTL_USER_PROCESS_PARAMETERS;
  PRTLUserProcessParameters = ^TRTLUserProcessParameters;

  PEB = record
    Reserved1         : array[0..2-1] of byte;
    BeingDebugged     : byte;
    Reserved2         : array[0..1-1] of byte;
    Reserved3         : array[0..2-1] of PVOID;
    Ldr               : PVOID;
    ProcessParameters : PRTLUserProcessParameters;
  end;
  TPEB = PEB;
  PPEB = ^TPEB;

  _PROCESS_BASIC_INFORMATION = record
    Reserved1                    : PVOID;
    PebBaseAddress               : PPEB;
    Reserved2                    : array[0..1] of PVOID;
    UniqueProcessId              : ULONG_PTR;
    Reserved3                    : PVOID;
  end;
  TProcessBasicInformation = _PROCESS_BASIC_INFORMATION;
  PProcessBasicInformation = ^TProcessBasicInformation;

  (* DbgHelp.dll *)
  MINIDUMP_EXCEPTION_INFORMATION = record
    ThreadId          : DWORD;
    ExceptionPointers : PExceptionPointers;
    ClientPointers    : BOOL;
  end;
  TMiniDumpExceptionInformation = MINIDUMP_EXCEPTION_INFORMATION;
  PMiniDumpExceptionInformation = ^TMiniDumpExceptionInformation;

  MINIDUMP_USER_STREAM = record
    Type_      : ULONG;
    BufferSize : ULONG;
    Buffer     : Pointer;
  end;
  TMiniDumpUserStream = MINIDUMP_USER_STREAM;
  PMiniDumpUserStream = ^TMiniDumpUserStream;

  MINIDUMP_USER_STREAM_INFORMATION = record
    UserStreamCount : ULONG;
    UserStreamArray : PMiniDumpUserStream;
  end;
  TMiniDumpUserStreamInformation = MINIDUMP_USER_STREAM_INFORMATION;
  PMiniDumpUserStreamInformation = ^TMiniDumpUserStreamInformation;

  TMiniDumpCallbackRoutine = function(
    CallbackParam      : Pointer;
    CallbackInput      : Pointer;
    var CallbackOutput : Pointer
  ): BOOL; stdcall;

  MINIDUMP_CALLBACK_INFORMATION = record
    CallbackRoutine : TMiniDumpCallbackRoutine;
    CallbackParam   : Pointer;
  end;
  TMiniDumpCallbackInformation = MINIDUMP_CALLBACK_INFORMATION;
  PMiniDumpCallbackInformation = ^TMiniDumpCallbackInformation;

  (* Ws2_32.dll *)
  TIn6Addr = record
    Byte: array[0..15] of Byte;
  end;
  PIn6Addr = ^TIn6Addr;

  TSockAddrIn6 = record
    sin6_family   : USHORT;
    sin6_port     : USHORT;
    sin6_flowinfo : ULONG;
    sin6_addr     : TIn6Addr;
    sin6_scope_id : ULONG;
  end;
  PSockAddrIn6 = ^TSockAddrIn6;

//----------------------------------------------------------------------------------------------------------------------

const
  NERR_Success = 0;

  INVALID_SET_FILE_POINTER = $FFFFFFFF;

  DS_DIRECTORY_SERVICE_REQUIRED = $00000010;

  SECURITY_NT_AUTHORITY : TSIDIdentifierAuthority = (
    Value: (0, 0, 0, 0, 0, 5)
  );

  DOMAIN_ALIAS_RID_ADMINS       = $00000220;
  SECURITY_BUILTIN_DOMAIN_RID   = $00000020;

  SYSTEM_PROCESS_INFORMATION_CLASS  = 5;
  PROCESS_QUERY_LIMITED_INFORMATION = $00001000;

  PROCESSOR_ARCHITECTURE_ARM64 = 12;

  SDDL_REVISION_1 = 1;

  FILE_GENERIC_READ = STANDARD_RIGHTS_READ or
                      FILE_READ_DATA or
                      FILE_READ_ATTRIBUTES or
                      FILE_READ_EA or
                      SYNCHRONIZE;

  FILE_GENERIC_WRITE = STANDARD_RIGHTS_WRITE or
                       FILE_WRITE_DATA or
                       FILE_WRITE_ATTRIBUTES or
                       FILE_WRITE_EA or
                       FILE_APPEND_DATA or
                       SYNCHRONIZE;

  FILE_GENERIC_EXECUTE = STANDARD_RIGHTS_EXECUTE or
                         FILE_READ_ATTRIBUTES or
                         FILE_EXECUTE or
                         SYNCHRONIZE;

  FILE_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or
                    SYNCHRONIZE or
                    $1FF;

  CREATE_BREAKAWAY_FROM_JOB = $01000000;

  (* DbgHelp.DLL *)

  MiniDumpNormal                          = $00000000;
  MiniDumpWithDataSegs                    = $00000001;
  MiniDumpWithFullMemory                  = $00000002;
  MiniDumpWithHandleData                  = $00000004;
  MiniDumpFilterMemory                    = $00000008;
  MiniDumpScanMemory                      = $00000010;
  MiniDumpWithUnloadedModules             = $00000020;
  MiniDumpWithIndirectlyReferencedMemory  = $00000040;
  MiniDumpFilterModulePaths               = $00000080;
  MiniDumpWithProcessThreadData           = $00000100;
  MiniDumpWithPrivateReadWriteMemory      = $00000200;
  MiniDumpWithoutOptionalData             = $00000400;
  MiniDumpWithFullMemoryInfo              = $00000800;
  MiniDumpWithThreadInfo                  = $00001000;
  MiniDumpWithCodeSegs                    = $00002000;
  MiniDumpWithoutAuxiliaryState           = $00004000;
  MiniDumpWithFullAuxiliaryState          = $00008000;
  MiniDumpWithPrivateWriteCopyMemory      = $00010000;
  MiniDumpIgnoreInaccessibleMemory        = $00020000;
  MiniDumpWithTokenInformation            = $00040000;
  MiniDumpWithModuleHeaders               = $00080000;
  MiniDumpFilterTriage                    = $00100000;
  MiniDumpWithAvxXStateContext            = $00200000;
  MiniDumpWithIptTrace                    = $00400000;
  MiniDumpScanInaccessiblePartialPages    = $00800000;
  MiniDumpFilterWriteCombinedMemory       = $01000000;
  MiniDumpValidTypeFlags                  = $01ffffff;

  (* Ws2_32.dll *)
  in6addr_any : TIn6Addr = (Byte: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0));
  NI_MAXHOST             = 1025;

  (* Advapi32.dll *)
  RRF_RT_ANY = $0000FFFF;

//----------------------------------------------------------------------------------------------------------------------

(* Netapi32.dll *)
function DsGetDcNameW(
  ComputerName         : LPCWSTR;
  DomainName           : LPCWSTR;
  GUID                 : PGUID;
  SiteName             : LPCWSTR;
  Flags                : ULONG;
  out DomainControllerInfo : PDomainControllerInfo
) : NET_API_STATUS; stdcall; external 'Netapi32.dll';

function NetWkstaGetInfo(
  servername : LPWSTR;
  level      : DWORD;
  out bufptr : Pointer
) : NET_API_STATUS; stdcall; external 'Netapi32.dll';

function NetApiBufferFree(
  Buffer : Pointer
) : NET_API_STATUS; stdcall; external 'Netapi32.dll';

(* Advapi32.dll *)

function CheckTokenMembership(
  TokenHandle  : THandle;
  SIdToCheck   : PSID;
  var IsMember : Boolean
): BOOL; stdcall; external 'Advapi32.dll';

function ConvertSecurityDescriptorToStringSecurityDescriptorW(
  SecurityDescriptor           : PSecurityDescriptor;
  RequestedStringSDRevision    : DWORD;
  SecurityInformation          : SECURITY_INFORMATION;
  var StringSecurityDescriptor : LPWSTR;
  StringSecurityDescriptorLen  : PULONG
): BOOL; stdcall; external 'Advapi32.dll';

function RegGetValueW(
  hkey        : HKEY;
  lpSubKey    : LPCWSTR;
  lpValue     : LPCWSTR;
  dwFlags     : DWORD;
  var dwType  : DWORD;
  pvData      : PVOID;
  var pcbData : DWORD
) : LONG; stdcall; external 'Advapi32.dll';

(* NTDLL.dll *)

function NtQuerySystemInformation(
  SystemInformationClass  : DWORD;
  SystemInformation       : Pointer;
  SystemInformationLength : DWORD;
  var ReturnLength        : DWORD
) : Cardinal; stdcall; external 'NTDLL.DLL';

function NtQueryInformationProcess(
  ProcessHandle            : THandle;
  ProcessInformationClass  : TProcessInformationClass;
  ProcessInformation       : Pointer;
  ProcessInformationLength : ULONG;
  var ReturnLength         : ULONG
) : NTSTATUS; stdcall; external 'NTDLL.DLL';

(* Kernel32.dll *)

function QueryFullProcessImageNameW(
  hProcess   : THandle;
  dwFlags    : DWORD;
  lpExeName  : PWideChar;
  var dwSize : DWORD
): BOOL; stdcall; external 'Kernel32.dll';

(* DbgHelp.dll *)
function MiniDumpWriteDump(
  hProcess        : THandle;
  ProcessId       : DWORD;
  hFile           : THandle;
  DumpType        : DWORD;
  ExceptionParam  : PMiniDumpExceptionInformation;
  UserStreamParam : PMiniDumpUserStreamInformation;
  CallbackParam   : PMiniDumpCallbackInformation
) : BOOL; stdcall; external 'DbgHelp.dll';

//----------------------------------------------------------------------------------------------------------------------

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
