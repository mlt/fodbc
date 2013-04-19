! DO NOT EDIT!
! Generated from gccxml using sql.xslt
! gccxml /usr/include/sqlext.h -fxml=/tmp/sqlext.xml && xsltproc sql.xslt /tmp/sqlext.xml sql.xslt > fodbc.f03
!
! This file is a part of fodbc, Fortran 2003 ODBC bindings
! Copyright (C) 2013 Mikhail Titov
!
! fodbc is free software: you can redistribute it and/or modify
! it under the terms of the GNU Affero General Public License as
! published by the Free Software Foundation, either version 3 of the
! License, or (at your option) any later version.
!
! fodbc is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Affero General Public License for more details.
!
! You should have received a copy of the GNU Affero General Public License
! along with fodbc.  If not, see <http://www.gnu.org/licenses/>.

module fodbc
  use fodbc_types
  implicit none

  interface SQLParamOptions
    integer(kind=c_short) function SQLParamOptions0 &
      (hstmt,crow,pirow) &
      bind(C, name="SQLParamOptions")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_long),intent(in),value :: crow
      integer(kind=c_long),intent(out) :: pirow
    end function SQLParamOptions0
  end interface SQLParamOptions

  interface SQLGetInfo
    integer(kind=c_short) function SQLGetInfo0 &
      (ConnectionHandle,InfoType,InfoValue,BufferLength,StringLength) &
      bind(C, name="SQLGetInfo")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
      integer(kind=c_short),intent(in),value :: InfoType
      type(c_ptr),intent(in),value :: InfoValue
      integer(kind=c_short),intent(in),value :: BufferLength
      integer(kind=c_short),intent(out) :: StringLength
    end function SQLGetInfo0
  end interface SQLGetInfo

  interface SQLSetStmtAttrW
    integer(kind=c_short) function SQLSetStmtAttrW0 &
      (hstmt,fAttribute,rgbValue,cbValueMax) &
      bind(C, name="SQLSetStmtAttrW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_long),intent(in),value :: fAttribute
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValueMax
    end function SQLSetStmtAttrW0
  end interface SQLSetStmtAttrW

  interface SQLExecDirectA
    integer(kind=c_short) function SQLExecDirectA0 &
      (hstmt,szSqlStr,cbSqlStr) &
      bind(C, name="SQLExecDirectA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szSqlStr
      integer(kind=c_long),intent(in),value :: cbSqlStr
    end function SQLExecDirectA0
  end interface SQLExecDirectA

  interface SQLNativeSql
    integer(kind=c_short) function SQLNativeSql0 &
      (hdbc,szSqlStrIn,cbSqlStrIn,szSqlStr,cbSqlStrMax, &
      pcbSqlStr) &
      bind(C, name="SQLNativeSql")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      character(kind=c_char) :: szSqlStrIn
      integer(kind=c_long),intent(in),value :: cbSqlStrIn
      character(kind=c_char) :: szSqlStr
      integer(kind=c_long),intent(in),value :: cbSqlStrMax
      integer(kind=c_long),intent(out) :: pcbSqlStr
    end function SQLNativeSql0
  end interface SQLNativeSql

  interface SQLGetConnectOptionW
    integer(kind=c_short) function SQLGetConnectOptionW0 &
      (hdbc,fOption,pvParam) &
      bind(C, name="SQLGetConnectOptionW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_short),intent(in),value :: fOption
      type(c_ptr),intent(in),value :: pvParam
    end function SQLGetConnectOptionW0
  end interface SQLGetConnectOptionW

  interface SQLDataSourcesA
    integer(kind=c_short) function SQLDataSourcesA0 &
      (henv,fDirection,szDSN,cbDSNMax,pcbDSN, &
      szDescription,cbDescriptionMax,pcbDescription) &
      bind(C, name="SQLDataSourcesA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: henv
      integer(kind=c_short),intent(in),value :: fDirection
      character(kind=c_char) :: szDSN
      integer(kind=c_short),intent(in),value :: cbDSNMax
      integer(kind=c_short),intent(out) :: pcbDSN
      character(kind=c_char) :: szDescription
      integer(kind=c_short),intent(in),value :: cbDescriptionMax
      integer(kind=c_short),intent(out) :: pcbDescription
    end function SQLDataSourcesA0
  end interface SQLDataSourcesA

  interface ODBCGetTryWaitValue
    integer(kind=c_long) function ODBCGetTryWaitValue0 &
      () &
      bind(C, name="ODBCGetTryWaitValue")
      use, intrinsic :: iso_c_binding
    end function ODBCGetTryWaitValue0
  end interface ODBCGetTryWaitValue

  interface SQLDataSourcesW
    integer(kind=c_short) function SQLDataSourcesW0 &
      (henv,fDirection,szDSN,cbDSNMax,pcbDSN, &
      szDescription,cbDescriptionMax,pcbDescription) &
      bind(C, name="SQLDataSourcesW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: henv
      integer(kind=c_short),intent(in),value :: fDirection
      integer(kind=c_short),intent(out) :: szDSN
      integer(kind=c_short),intent(in),value :: cbDSNMax
      integer(kind=c_short),intent(out) :: pcbDSN
      integer(kind=c_short),intent(out) :: szDescription
      integer(kind=c_short),intent(in),value :: cbDescriptionMax
      integer(kind=c_short),intent(out) :: pcbDescription
    end function SQLDataSourcesW0
  end interface SQLDataSourcesW

  interface SQLCloseCursor
    integer(kind=c_short) function SQLCloseCursor0 &
      (StatementHandle) &
      bind(C, name="SQLCloseCursor")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
    end function SQLCloseCursor0
  end interface SQLCloseCursor

  interface SQLSetEnvAttr
    integer(kind=c_short) function SQLSetEnvAttr0 &
      (EnvironmentHandle,Attribute,Value,StringLength) &
      bind(C, name="SQLSetEnvAttr")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: EnvironmentHandle
      integer(kind=c_long),intent(in),value :: Attribute
      type(c_ptr),intent(in),value :: Value
      integer(kind=c_long),intent(in),value :: StringLength
    end function SQLSetEnvAttr0
  end interface SQLSetEnvAttr

  interface SQLFetchScroll
    integer(kind=c_short) function SQLFetchScroll0 &
      (StatementHandle,FetchOrientation,FetchOffset) &
      bind(C, name="SQLFetchScroll")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: FetchOrientation
      integer(kind=c_long),intent(in),value :: FetchOffset
    end function SQLFetchScroll0
  end interface SQLFetchScroll

  interface SQLFreeEnv
    integer(kind=c_short) function SQLFreeEnv0 &
      (EnvironmentHandle) &
      bind(C, name="SQLFreeEnv")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: EnvironmentHandle
    end function SQLFreeEnv0
  end interface SQLFreeEnv

  interface SQLGetFunctions
    integer(kind=c_short) function SQLGetFunctions0 &
      (ConnectionHandle,FunctionId,Supported) &
      bind(C, name="SQLGetFunctions")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
      integer(kind=c_short),intent(in),value :: FunctionId
      integer(kind=c_short),intent(out) :: Supported
    end function SQLGetFunctions0
  end interface SQLGetFunctions

  interface SQLSetParam
    integer(kind=c_short) function SQLSetParam0 &
      (StatementHandle,ParameterNumber,ValueType,ParameterType,LengthPrecision, &
      ParameterScale,ParameterValue,StrLen_or_Ind) &
      bind(C, name="SQLSetParam")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: ParameterNumber
      integer(kind=c_short),intent(in),value :: ValueType
      integer(kind=c_short),intent(in),value :: ParameterType
      integer(kind=c_long),intent(in),value :: LengthPrecision
      integer(kind=c_short),intent(in),value :: ParameterScale
      type(c_ptr),intent(in),value :: ParameterValue
      integer(kind=c_long),intent(out) :: StrLen_or_Ind
    end function SQLSetParam0
  end interface SQLSetParam

  interface SQLEndTran
    integer(kind=c_short) function SQLEndTran0 &
      (HandleType,Handle,CompletionType) &
      bind(C, name="SQLEndTran")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: HandleType
      type(c_ptr),intent(in),value :: Handle
      integer(kind=c_short),intent(in),value :: CompletionType
    end function SQLEndTran0
  end interface SQLEndTran

  interface SQLColumns
    integer(kind=c_short) function SQLColumns0 &
      (StatementHandle,CatalogName,NameLength1,SchemaName,NameLength2, &
      TableName,NameLength3,ColumnName,NameLength4) &
      bind(C, name="SQLColumns")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      character(kind=c_char) :: CatalogName
      integer(kind=c_short),intent(in),value :: NameLength1
      character(kind=c_char) :: SchemaName
      integer(kind=c_short),intent(in),value :: NameLength2
      character(kind=c_char) :: TableName
      integer(kind=c_short),intent(in),value :: NameLength3
      character(kind=c_char) :: ColumnName
      integer(kind=c_short),intent(in),value :: NameLength4
    end function SQLColumns0
  end interface SQLColumns

  interface SQLSetConnectAttr
    integer(kind=c_short) function SQLSetConnectAttr0 &
      (ConnectionHandle,Attribute,Value,StringLength) &
      bind(C, name="SQLSetConnectAttr")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
      integer(kind=c_long),intent(in),value :: Attribute
      type(c_ptr),intent(in),value :: Value
      integer(kind=c_long),intent(in),value :: StringLength
    end function SQLSetConnectAttr0
  end interface SQLSetConnectAttr

  interface SQLError
    integer(kind=c_short) function SQLError0 &
      (EnvironmentHandle,ConnectionHandle,StatementHandle,Sqlstate,NativeError, &
      MessageText,BufferLength,TextLength) &
      bind(C, name="SQLError")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: EnvironmentHandle
      type(c_ptr),intent(in),value :: ConnectionHandle
      type(c_ptr),intent(in),value :: StatementHandle
      character(kind=c_char) :: Sqlstate
      integer(kind=c_long),intent(out) :: NativeError
      character(kind=c_char) :: MessageText
      integer(kind=c_short),intent(in),value :: BufferLength
      integer(kind=c_short),intent(out) :: TextLength
    end function SQLError0
  end interface SQLError

  interface SQLSetScrollOptions
    integer(kind=c_short) function SQLSetScrollOptions0 &
      (hstmt,fConcurrency,crowKeyset,crowRowset) &
      bind(C, name="SQLSetScrollOptions")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: fConcurrency
      integer(kind=c_long),intent(in),value :: crowKeyset
      integer(kind=c_short),intent(in),value :: crowRowset
    end function SQLSetScrollOptions0
  end interface SQLSetScrollOptions

  interface TraceVSControl
    integer(kind=c_short) function TraceVSControl0 &
      (var1) &
      bind(C, name="TraceVSControl")
      use, intrinsic :: iso_c_binding
      integer(kind=c_long),intent(in),value :: var1
    end function TraceVSControl0
  end interface TraceVSControl

  interface SQLGetStmtAttrA
    integer(kind=c_short) function SQLGetStmtAttrA0 &
      (hstmt,fAttribute,rgbValue,cbValueMax,pcbValue) &
      bind(C, name="SQLGetStmtAttrA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_long),intent(in),value :: fAttribute
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValueMax
      integer(kind=c_long),intent(out) :: pcbValue
    end function SQLGetStmtAttrA0
  end interface SQLGetStmtAttrA

  interface SQLProcedureColumnsA
    integer(kind=c_short) function SQLProcedureColumnsA0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szProcName,cbProcName,szColumnName,cbColumnName) &
      bind(C, name="SQLProcedureColumnsA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char) :: szProcName
      integer(kind=c_short),intent(in),value :: cbProcName
      character(kind=c_char) :: szColumnName
      integer(kind=c_short),intent(in),value :: cbColumnName
    end function SQLProcedureColumnsA0
  end interface SQLProcedureColumnsA

  interface SQLGetCursorName
    integer(kind=c_short) function SQLGetCursorName0 &
      (StatementHandle,CursorName,BufferLength,NameLength) &
      bind(C, name="SQLGetCursorName")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      character(kind=c_char) :: CursorName
      integer(kind=c_short),intent(in),value :: BufferLength
      integer(kind=c_short),intent(out) :: NameLength
    end function SQLGetCursorName0
  end interface SQLGetCursorName

  interface SQLPrepare
    integer(kind=c_short) function SQLPrepare0 &
      (StatementHandle,StatementText,TextLength) &
      bind(C, name="SQLPrepare")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      character(kind=c_char) :: StatementText
      integer(kind=c_long),intent(in),value :: TextLength
    end function SQLPrepare0
  end interface SQLPrepare

  interface SQLTablePrivilegesA
    integer(kind=c_short) function SQLTablePrivilegesA0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName) &
      bind(C, name="SQLTablePrivilegesA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
    end function SQLTablePrivilegesA0
  end interface SQLTablePrivilegesA

  interface SQLAllocEnv
    integer(kind=c_short) function SQLAllocEnv0 &
      (EnvironmentHandle) &
      bind(C, name="SQLAllocEnv")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(out) :: EnvironmentHandle
    end function SQLAllocEnv0
  end interface SQLAllocEnv

  interface SQLTablePrivilegesW
    integer(kind=c_short) function SQLTablePrivilegesW0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName) &
      bind(C, name="SQLTablePrivilegesW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      integer(kind=c_short),intent(out) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      integer(kind=c_short),intent(out) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
    end function SQLTablePrivilegesW0
  end interface SQLTablePrivilegesW

  interface SQLGetInfoA
    integer(kind=c_short) function SQLGetInfoA0 &
      (hdbc,fInfoType,rgbInfoValue,cbInfoValueMax,pcbInfoValue) &
      bind(C, name="SQLGetInfoA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_short),intent(in),value :: fInfoType
      type(c_ptr),intent(in),value :: rgbInfoValue
      integer(kind=c_short),intent(in),value :: cbInfoValueMax
      integer(kind=c_short),intent(out) :: pcbInfoValue
    end function SQLGetInfoA0
  end interface SQLGetInfoA

  interface SQLGetInfoW
    integer(kind=c_short) function SQLGetInfoW0 &
      (hdbc,fInfoType,rgbInfoValue,cbInfoValueMax,pcbInfoValue) &
      bind(C, name="SQLGetInfoW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_short),intent(in),value :: fInfoType
      type(c_ptr),intent(in),value :: rgbInfoValue
      integer(kind=c_short),intent(in),value :: cbInfoValueMax
      integer(kind=c_short),intent(out) :: pcbInfoValue
    end function SQLGetInfoW0
  end interface SQLGetInfoW

  interface SQLConnect
    integer(kind=c_short) function SQLConnect0 &
      (ConnectionHandle,ServerName,NameLength1,UserName,NameLength2, &
      Authentication,NameLength3) &
      bind(C, name="SQLConnect")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
      character(kind=c_char) :: ServerName
      integer(kind=c_short),intent(in),value :: NameLength1
      character(kind=c_char) :: UserName
      integer(kind=c_short),intent(in),value :: NameLength2
      character(kind=c_char) :: Authentication
      integer(kind=c_short),intent(in),value :: NameLength3
    end function SQLConnect0
  end interface SQLConnect

  interface SQLGetConnectAttrA
    integer(kind=c_short) function SQLGetConnectAttrA0 &
      (hdbc,fAttribute,rgbValue,cbValueMax,pcbValue) &
      bind(C, name="SQLGetConnectAttrA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_long),intent(in),value :: fAttribute
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValueMax
      integer(kind=c_long),intent(out) :: pcbValue
    end function SQLGetConnectAttrA0
  end interface SQLGetConnectAttrA

  interface SQLColAttributeW
    integer(kind=c_short) function SQLColAttributeW0 &
      (hstmt,iCol,iField,pCharAttr,cbCharAttrMax, &
      pcbCharAttr,pNumAttr) &
      bind(C, name="SQLColAttributeW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: iCol
      integer(kind=c_short),intent(in),value :: iField
      type(c_ptr),intent(in),value :: pCharAttr
      integer(kind=c_short),intent(in),value :: cbCharAttrMax
      integer(kind=c_short),intent(out) :: pcbCharAttr
      integer(kind=c_long),intent(out) :: pNumAttr
    end function SQLColAttributeW0
  end interface SQLColAttributeW

  interface SQLColAttributes
    integer(kind=c_short) function SQLColAttributes0 &
      (hstmt,icol,fDescType,rgbDesc,cbDescMax, &
      pcbDesc,pfDesc) &
      bind(C, name="SQLColAttributes")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: icol
      integer(kind=c_short),intent(in),value :: fDescType
      type(c_ptr),intent(in),value :: rgbDesc
      integer(kind=c_short),intent(in),value :: cbDescMax
      integer(kind=c_short),intent(out) :: pcbDesc
      integer(kind=c_long),intent(out) :: pfDesc
    end function SQLColAttributes0
  end interface SQLColAttributes

  interface SQLPrepareA
    integer(kind=c_short) function SQLPrepareA0 &
      (hstmt,szSqlStr,cbSqlStr) &
      bind(C, name="SQLPrepareA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szSqlStr
      integer(kind=c_long),intent(in),value :: cbSqlStr
    end function SQLPrepareA0
  end interface SQLPrepareA

  interface SQLPrepareW
    integer(kind=c_short) function SQLPrepareW0 &
      (hstmt,szSqlStr,cbSqlStr) &
      bind(C, name="SQLPrepareW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szSqlStr
      integer(kind=c_long),intent(in),value :: cbSqlStr
    end function SQLPrepareW0
  end interface SQLPrepareW

  interface SQLGetStmtAttrW
    integer(kind=c_short) function SQLGetStmtAttrW0 &
      (hstmt,fAttribute,rgbValue,cbValueMax,pcbValue) &
      bind(C, name="SQLGetStmtAttrW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_long),intent(in),value :: fAttribute
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValueMax
      integer(kind=c_long),intent(out) :: pcbValue
    end function SQLGetStmtAttrW0
  end interface SQLGetStmtAttrW

  interface SQLGetDiagFieldA
    integer(kind=c_short) function SQLGetDiagFieldA0 &
      (fHandleType,handle,iRecord,fDiagField,rgbDiagInfo, &
      cbDiagInfoMax,pcbDiagInfo) &
      bind(C, name="SQLGetDiagFieldA")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: fHandleType
      type(c_ptr),intent(in),value :: handle
      integer(kind=c_short),intent(in),value :: iRecord
      integer(kind=c_short),intent(in),value :: fDiagField
      type(c_ptr),intent(in),value :: rgbDiagInfo
      integer(kind=c_short),intent(in),value :: cbDiagInfoMax
      integer(kind=c_short),intent(out) :: pcbDiagInfo
    end function SQLGetDiagFieldA0
  end interface SQLGetDiagFieldA

  interface SQLGetDiagFieldW
    integer(kind=c_short) function SQLGetDiagFieldW0 &
      (fHandleType,handle,iRecord,fDiagField,rgbDiagInfo, &
      cbDiagInfoMax,pcbDiagInfo) &
      bind(C, name="SQLGetDiagFieldW")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: fHandleType
      type(c_ptr),intent(in),value :: handle
      integer(kind=c_short),intent(in),value :: iRecord
      integer(kind=c_short),intent(in),value :: fDiagField
      type(c_ptr),intent(in),value :: rgbDiagInfo
      integer(kind=c_short),intent(in),value :: cbDiagInfoMax
      integer(kind=c_short),intent(out) :: pcbDiagInfo
    end function SQLGetDiagFieldW0
  end interface SQLGetDiagFieldW

  interface SQLBrowseConnect
    integer(kind=c_short) function SQLBrowseConnect0 &
      (hdbc,szConnStrIn,cbConnStrIn,szConnStrOut,cbConnStrOutMax, &
      pcbConnStrOut) &
      bind(C, name="SQLBrowseConnect")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      character(kind=c_char) :: szConnStrIn
      integer(kind=c_short),intent(in),value :: cbConnStrIn
      character(kind=c_char) :: szConnStrOut
      integer(kind=c_short),intent(in),value :: cbConnStrOutMax
      integer(kind=c_short),intent(out) :: pcbConnStrOut
    end function SQLBrowseConnect0
  end interface SQLBrowseConnect

  interface SQLExecute
    integer(kind=c_short) function SQLExecute0 &
      (StatementHandle) &
      bind(C, name="SQLExecute")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
    end function SQLExecute0
  end interface SQLExecute

  interface TraceOpenLogFile
    integer(kind=c_short) function TraceOpenLogFile0 &
      (var1,var2,var3,var4) &
      bind(C, name="TraceOpenLogFile")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: var1
      character(kind=c_char) :: var2
      character(kind=c_char) :: var3
      integer(kind=c_long),intent(in),value :: var4
    end function TraceOpenLogFile0
  end interface TraceOpenLogFile

  interface SQLExtendedFetch
    integer(kind=c_short) function SQLExtendedFetch0 &
      (hstmt,fFetchType,irow,pcrow,rgfRowStatus) &
      bind(C, name="SQLExtendedFetch")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: fFetchType
      integer(kind=c_long),intent(in),value :: irow
      integer(kind=c_long),intent(out) :: pcrow
      integer(kind=c_short),intent(out) :: rgfRowStatus
    end function SQLExtendedFetch0
  end interface SQLExtendedFetch

  interface SQLDrivers
    integer(kind=c_short) function SQLDrivers0 &
      (henv,fDirection,szDriverDesc,cbDriverDescMax,pcbDriverDesc, &
      szDriverAttributes,cbDrvrAttrMax,pcbDrvrAttr) &
      bind(C, name="SQLDrivers")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: henv
      integer(kind=c_short),intent(in),value :: fDirection
      character(kind=c_char) :: szDriverDesc
      integer(kind=c_short),intent(in),value :: cbDriverDescMax
      integer(kind=c_short),intent(out) :: pcbDriverDesc
      character(kind=c_char) :: szDriverAttributes
      integer(kind=c_short),intent(in),value :: cbDrvrAttrMax
      integer(kind=c_short),intent(out) :: pcbDrvrAttr
    end function SQLDrivers0
  end interface SQLDrivers

  interface SQLSpecialColumns
    integer(kind=c_short) function SQLSpecialColumns0 &
      (StatementHandle,IdentifierType,CatalogName,NameLength1,SchemaName, &
      NameLength2,TableName,NameLength3,Scope,Nullable) &
      bind(C, name="SQLSpecialColumns")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: IdentifierType
      character(kind=c_char) :: CatalogName
      integer(kind=c_short),intent(in),value :: NameLength1
      character(kind=c_char) :: SchemaName
      integer(kind=c_short),intent(in),value :: NameLength2
      character(kind=c_char) :: TableName
      integer(kind=c_short),intent(in),value :: NameLength3
      integer(kind=c_short),intent(in),value :: Scope
      integer(kind=c_short),intent(in),value :: Nullable
    end function SQLSpecialColumns0
  end interface SQLSpecialColumns

  interface SQLRowCount
    integer(kind=c_short) function SQLRowCount0 &
      (StatementHandle,RowCount) &
      bind(C, name="SQLRowCount")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_long),intent(out) :: RowCount
    end function SQLRowCount0
  end interface SQLRowCount

  interface SQLDescribeCol
    integer(kind=c_short) function SQLDescribeCol0 &
      (StatementHandle,ColumnNumber,ColumnName,BufferLength,NameLength, &
      DataType,ColumnSize,DecimalDigits,Nullable) &
      bind(C, name="SQLDescribeCol")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: ColumnNumber
      character(kind=c_char) :: ColumnName
      integer(kind=c_short),intent(in),value :: BufferLength
      integer(kind=c_short),intent(out) :: NameLength
      integer(kind=c_short),intent(out) :: DataType
      integer(kind=c_long),intent(out) :: ColumnSize
      integer(kind=c_short),intent(out) :: DecimalDigits
      integer(kind=c_short),intent(out) :: Nullable
    end function SQLDescribeCol0
  end interface SQLDescribeCol

  interface SQLBindCol
    integer(kind=c_short) function SQLBindCol0 &
      (StatementHandle,ColumnNumber,TargetType,TargetValue,BufferLength, &
      StrLen_or_Ind) &
      bind(C, name="SQLBindCol")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: ColumnNumber
      integer(kind=c_short),intent(in),value :: TargetType
      type(c_ptr),intent(in),value :: TargetValue
      integer(kind=c_long),intent(in),value :: BufferLength
      integer(kind=c_long),intent(out) :: StrLen_or_Ind
    end function SQLBindCol0
    module procedure SQLBindColInt
    module procedure SQLBindColReal
    module procedure SQLBindColDouble
    module procedure SQLBindColTimeStamp
    module procedure SQLBindColDate
  end interface SQLBindCol

  interface SQLForeignKeysA
    integer(kind=c_short) function SQLForeignKeysA0 &
      (hstmt,szPkCatalogName,cbPkCatalogName,szPkSchemaName,cbPkSchemaName, &
      szPkTableName,cbPkTableName,szFkCatalogName,cbFkCatalogName,szFkSchemaName, &
      cbFkSchemaName,szFkTableName,cbFkTableName) &
      bind(C, name="SQLForeignKeysA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szPkCatalogName
      integer(kind=c_short),intent(in),value :: cbPkCatalogName
      character(kind=c_char) :: szPkSchemaName
      integer(kind=c_short),intent(in),value :: cbPkSchemaName
      character(kind=c_char) :: szPkTableName
      integer(kind=c_short),intent(in),value :: cbPkTableName
      character(kind=c_char) :: szFkCatalogName
      integer(kind=c_short),intent(in),value :: cbFkCatalogName
      character(kind=c_char) :: szFkSchemaName
      integer(kind=c_short),intent(in),value :: cbFkSchemaName
      character(kind=c_char) :: szFkTableName
      integer(kind=c_short),intent(in),value :: cbFkTableName
    end function SQLForeignKeysA0
  end interface SQLForeignKeysA

  interface SQLGetDiagRec
    integer(kind=c_short) function SQLGetDiagRec0 &
      (HandleType,Handle,RecNumber,Sqlstate,NativeError, &
      MessageText,BufferLength,TextLength) &
      bind(C, name="SQLGetDiagRec")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: HandleType
      type(c_ptr),intent(in),value :: Handle
      integer(kind=c_short),intent(in),value :: RecNumber
      character(kind=c_char) :: Sqlstate
      integer(kind=c_long),intent(out) :: NativeError
      character(kind=c_char) :: MessageText
      integer(kind=c_short),intent(in),value :: BufferLength
      integer(kind=c_short),intent(out) :: TextLength
    end function SQLGetDiagRec0
  end interface SQLGetDiagRec

  interface SQLFreeConnect
    integer(kind=c_short) function SQLFreeConnect0 &
      (ConnectionHandle) &
      bind(C, name="SQLFreeConnect")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
    end function SQLFreeConnect0
  end interface SQLFreeConnect

  interface SQLGetCursorNameA
    integer(kind=c_short) function SQLGetCursorNameA0 &
      (hstmt,szCursor,cbCursorMax,pcbCursor) &
      bind(C, name="SQLGetCursorNameA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szCursor
      integer(kind=c_short),intent(in),value :: cbCursorMax
      integer(kind=c_short),intent(out) :: pcbCursor
    end function SQLGetCursorNameA0
  end interface SQLGetCursorNameA

  interface SQLTablePrivileges
    integer(kind=c_short) function SQLTablePrivileges0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName) &
      bind(C, name="SQLTablePrivileges")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
    end function SQLTablePrivileges0
  end interface SQLTablePrivileges

  interface SQLDataSources
    integer(kind=c_short) function SQLDataSources0 &
      (EnvironmentHandle,Direction,ServerName,BufferLength1,NameLength1, &
      Description,BufferLength2,NameLength2) &
      bind(C, name="SQLDataSources")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: EnvironmentHandle
      integer(kind=c_short),intent(in),value :: Direction
      character(kind=c_char) :: ServerName
      integer(kind=c_short),intent(in),value :: BufferLength1
      integer(kind=c_short),intent(out) :: NameLength1
      character(kind=c_char) :: Description
      integer(kind=c_short),intent(in),value :: BufferLength2
      integer(kind=c_short),intent(out) :: NameLength2
    end function SQLDataSources0
  end interface SQLDataSources

  interface SQLGetDiagField
    integer(kind=c_short) function SQLGetDiagField0 &
      (HandleType,Handle,RecNumber,DiagIdentifier,DiagInfo, &
      BufferLength,StringLength) &
      bind(C, name="SQLGetDiagField")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: HandleType
      type(c_ptr),intent(in),value :: Handle
      integer(kind=c_short),intent(in),value :: RecNumber
      integer(kind=c_short),intent(in),value :: DiagIdentifier
      type(c_ptr),intent(in),value :: DiagInfo
      integer(kind=c_short),intent(in),value :: BufferLength
      integer(kind=c_short),intent(out) :: StringLength
    end function SQLGetDiagField0
  end interface SQLGetDiagField

  interface SQLGetDescRec
    integer(kind=c_short) function SQLGetDescRec0 &
      (DescriptorHandle,RecNumber,Name,BufferLength,StringLength, &
      Type,SubType,Length,Precision,Scale, &
      Nullable) &
      bind(C, name="SQLGetDescRec")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: DescriptorHandle
      integer(kind=c_short),intent(in),value :: RecNumber
      character(kind=c_char) :: Name
      integer(kind=c_short),intent(in),value :: BufferLength
      integer(kind=c_short),intent(out) :: StringLength
      integer(kind=c_short),intent(out) :: Type
      integer(kind=c_short),intent(out) :: SubType
      integer(kind=c_long),intent(out) :: Length
      integer(kind=c_short),intent(out) :: Precision
      integer(kind=c_short),intent(out) :: Scale
      integer(kind=c_short),intent(out) :: Nullable
    end function SQLGetDescRec0
  end interface SQLGetDescRec

  interface SQLDescribeColA
    integer(kind=c_short) function SQLDescribeColA0 &
      (hstmt,icol,szColName,cbColNameMax,pcbColName, &
      pfSqlType,pcbColDef,pibScale,pfNullable) &
      bind(C, name="SQLDescribeColA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: icol
      character(kind=c_char) :: szColName
      integer(kind=c_short),intent(in),value :: cbColNameMax
      integer(kind=c_short),intent(out) :: pcbColName
      integer(kind=c_short),intent(out) :: pfSqlType
      integer(kind=c_long),intent(out) :: pcbColDef
      integer(kind=c_short),intent(out) :: pibScale
      integer(kind=c_short),intent(out) :: pfNullable
    end function SQLDescribeColA0
  end interface SQLDescribeColA

  interface TraceVersion
    integer(kind=c_long) function TraceVersion0 &
      () &
      bind(C, name="TraceVersion")
      use, intrinsic :: iso_c_binding
    end function TraceVersion0
  end interface TraceVersion

  interface SQLSetConnectOptionW
    integer(kind=c_short) function SQLSetConnectOptionW0 &
      (hdbc,fOption,vParam) &
      bind(C, name="SQLSetConnectOptionW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_short),intent(in),value :: fOption
      integer(kind=c_long),intent(in),value :: vParam
    end function SQLSetConnectOptionW0
  end interface SQLSetConnectOptionW

  interface SQLBulkOperations
    integer(kind=c_short) function SQLBulkOperations0 &
      (StatementHandle,Operation) &
      bind(C, name="SQLBulkOperations")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: Operation
    end function SQLBulkOperations0
  end interface SQLBulkOperations

  interface SQLTablesA
    integer(kind=c_short) function SQLTablesA0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName,szTableType,cbTableType) &
      bind(C, name="SQLTablesA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      character(kind=c_char) :: szTableType
      integer(kind=c_short),intent(in),value :: cbTableType
    end function SQLTablesA0
  end interface SQLTablesA

  interface SQLGetTypeInfoA
    integer(kind=c_short) function SQLGetTypeInfoA0 &
      (StatementHandle,DataTyoe) &
      bind(C, name="SQLGetTypeInfoA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: DataTyoe
    end function SQLGetTypeInfoA0
  end interface SQLGetTypeInfoA

  interface SQLGetTypeInfoW
    integer(kind=c_short) function SQLGetTypeInfoW0 &
      (StatementHandle,DataType) &
      bind(C, name="SQLGetTypeInfoW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: DataType
    end function SQLGetTypeInfoW0
  end interface SQLGetTypeInfoW

  interface SQLSetPos
    integer(kind=c_short) function SQLSetPos0 &
      (hstmt,irow,fOption,fLock) &
      bind(C, name="SQLSetPos")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: irow
      integer(kind=c_short),intent(in),value :: fOption
      integer(kind=c_short),intent(in),value :: fLock
    end function SQLSetPos0
  end interface SQLSetPos

  interface SQLGetConnectOption
    integer(kind=c_short) function SQLGetConnectOption0 &
      (ConnectionHandle,Option,Value) &
      bind(C, name="SQLGetConnectOption")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
      integer(kind=c_short),intent(in),value :: Option
      type(c_ptr),intent(in),value :: Value
    end function SQLGetConnectOption0
  end interface SQLGetConnectOption

  interface SQLSetStmtAttr
    integer(kind=c_short) function SQLSetStmtAttr0 &
      (StatementHandle,Attribute,Value,StringLength) &
      bind(C, name="SQLSetStmtAttr")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_long),intent(in),value :: Attribute
      type(c_ptr),intent(in),value :: Value
      integer(kind=c_long),intent(in),value :: StringLength
    end function SQLSetStmtAttr0
  end interface SQLSetStmtAttr

  interface SQLSetDescFieldW
    integer(kind=c_short) function SQLSetDescFieldW0 &
      (DescriptorHandle,RecNumber,FieldIdentifier,Value,BufferLength) &
      bind(C, name="SQLSetDescFieldW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: DescriptorHandle
      integer(kind=c_short),intent(in),value :: RecNumber
      integer(kind=c_short),intent(in),value :: FieldIdentifier
      type(c_ptr),intent(in),value :: Value
      integer(kind=c_long),intent(in),value :: BufferLength
    end function SQLSetDescFieldW0
  end interface SQLSetDescFieldW

  interface SQLFreeStmt
    integer(kind=c_short) function SQLFreeStmt0 &
      (StatementHandle,Option) &
      bind(C, name="SQLFreeStmt")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: Option
    end function SQLFreeStmt0
  end interface SQLFreeStmt

  interface SQLGetStmtOption
    integer(kind=c_short) function SQLGetStmtOption0 &
      (StatementHandle,Option,Value) &
      bind(C, name="SQLGetStmtOption")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: Option
      type(c_ptr),intent(in),value :: Value
    end function SQLGetStmtOption0
  end interface SQLGetStmtOption

  interface SQLFreeHandle
    integer(kind=c_short) function SQLFreeHandle0 &
      (HandleType,Handle) &
      bind(C, name="SQLFreeHandle")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: HandleType
      type(c_ptr),intent(in),value :: Handle
    end function SQLFreeHandle0
  end interface SQLFreeHandle

  interface SQLSetStmtOptionA
    integer(kind=c_short) function SQLSetStmtOptionA0 &
      (hstmt,fOption,vParam) &
      bind(C, name="SQLSetStmtOptionA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: fOption
      integer(kind=c_long),intent(in),value :: vParam
    end function SQLSetStmtOptionA0
  end interface SQLSetStmtOptionA

  interface SQLSetDescField
    integer(kind=c_short) function SQLSetDescField0 &
      (DescriptorHandle,RecNumber,FieldIdentifier,Value,BufferLength) &
      bind(C, name="SQLSetDescField")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: DescriptorHandle
      integer(kind=c_short),intent(in),value :: RecNumber
      integer(kind=c_short),intent(in),value :: FieldIdentifier
      type(c_ptr),intent(in),value :: Value
      integer(kind=c_long),intent(in),value :: BufferLength
    end function SQLSetDescField0
  end interface SQLSetDescField

  interface SQLDriverConnectA
    integer(kind=c_short) function SQLDriverConnectA0 &
      (hdbc,hwnd,szConnStrIn,cbConnStrIn,szConnStrOut, &
      cbConnStrOutMax,pcbConnStrOut,fDriverCompletion) &
      bind(C, name="SQLDriverConnectA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      type(c_ptr),intent(in),value :: hwnd
      character(kind=c_char) :: szConnStrIn
      integer(kind=c_short),intent(in),value :: cbConnStrIn
      character(kind=c_char) :: szConnStrOut
      integer(kind=c_short),intent(in),value :: cbConnStrOutMax
      integer(kind=c_short),intent(out) :: pcbConnStrOut
      integer(kind=c_short),intent(in),value :: fDriverCompletion
    end function SQLDriverConnectA0
  end interface SQLDriverConnectA

  interface SQLForeignKeysW
    integer(kind=c_short) function SQLForeignKeysW0 &
      (hstmt,szPkCatalogName,cbPkCatalogName,szPkSchemaName,cbPkSchemaName, &
      szPkTableName,cbPkTableName,szFkCatalogName,cbFkCatalogName,szFkSchemaName, &
      cbFkSchemaName,szFkTableName,cbFkTableName) &
      bind(C, name="SQLForeignKeysW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szPkCatalogName
      integer(kind=c_short),intent(in),value :: cbPkCatalogName
      integer(kind=c_short),intent(out) :: szPkSchemaName
      integer(kind=c_short),intent(in),value :: cbPkSchemaName
      integer(kind=c_short),intent(out) :: szPkTableName
      integer(kind=c_short),intent(in),value :: cbPkTableName
      integer(kind=c_short),intent(out) :: szFkCatalogName
      integer(kind=c_short),intent(in),value :: cbFkCatalogName
      integer(kind=c_short),intent(out) :: szFkSchemaName
      integer(kind=c_short),intent(in),value :: cbFkSchemaName
      integer(kind=c_short),intent(out) :: szFkTableName
      integer(kind=c_short),intent(in),value :: cbFkTableName
    end function SQLForeignKeysW0
  end interface SQLForeignKeysW

  interface SQLGetStmtAttr
    integer(kind=c_short) function SQLGetStmtAttr0 &
      (StatementHandle,Attribute,Value,BufferLength,StringLength) &
      bind(C, name="SQLGetStmtAttr")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_long),intent(in),value :: Attribute
      type(c_ptr),intent(in),value :: Value
      integer(kind=c_long),intent(in),value :: BufferLength
      integer(kind=c_long),intent(out) :: StringLength
    end function SQLGetStmtAttr0
  end interface SQLGetStmtAttr

  interface SQLBrowseConnectA
    integer(kind=c_short) function SQLBrowseConnectA0 &
      (hdbc,szConnStrIn,cbConnStrIn,szConnStrOut,cbConnStrOutMax, &
      pcbConnStrOut) &
      bind(C, name="SQLBrowseConnectA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      character(kind=c_char) :: szConnStrIn
      integer(kind=c_short),intent(in),value :: cbConnStrIn
      character(kind=c_char) :: szConnStrOut
      integer(kind=c_short),intent(in),value :: cbConnStrOutMax
      integer(kind=c_short),intent(out) :: pcbConnStrOut
    end function SQLBrowseConnectA0
  end interface SQLBrowseConnectA

  interface SQLParamData
    integer(kind=c_short) function SQLParamData0 &
      (StatementHandle,Value) &
      bind(C, name="SQLParamData")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      type(c_ptr),intent(out) :: Value
    end function SQLParamData0
  end interface SQLParamData

  interface SQLGetConnectAttr
    integer(kind=c_short) function SQLGetConnectAttr0 &
      (ConnectionHandle,Attribute,Value,BufferLength,StringLength) &
      bind(C, name="SQLGetConnectAttr")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
      integer(kind=c_long),intent(in),value :: Attribute
      type(c_ptr),intent(in),value :: Value
      integer(kind=c_long),intent(in),value :: BufferLength
      integer(kind=c_long),intent(out) :: StringLength
    end function SQLGetConnectAttr0
  end interface SQLGetConnectAttr

  interface SQLNumResultCols
    integer(kind=c_short) function SQLNumResultCols0 &
      (StatementHandle,ColumnCount) &
      bind(C, name="SQLNumResultCols")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(out) :: ColumnCount
    end function SQLNumResultCols0
  end interface SQLNumResultCols

  interface SQLAllocHandle
    integer(kind=c_short) function SQLAllocHandle0 &
      (HandleType,InputHandle,OutputHandle) &
      bind(C, name="SQLAllocHandle")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: HandleType
      type(c_ptr),intent(in),value :: InputHandle
      type(c_ptr),intent(out) :: OutputHandle
    end function SQLAllocHandle0
  end interface SQLAllocHandle

  interface SQLConnectA
    integer(kind=c_short) function SQLConnectA0 &
      (hdbc,szDSN,cbDSN,szUID,cbUID, &
      szAuthStr,cbAuthStr) &
      bind(C, name="SQLConnectA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      character(kind=c_char) :: szDSN
      integer(kind=c_short),intent(in),value :: cbDSN
      character(kind=c_char) :: szUID
      integer(kind=c_short),intent(in),value :: cbUID
      character(kind=c_char) :: szAuthStr
      integer(kind=c_short),intent(in),value :: cbAuthStr
    end function SQLConnectA0
  end interface SQLConnectA

  interface SQLConnectW
    integer(kind=c_short) function SQLConnectW0 &
      (hdbc,szDSN,cbDSN,szUID,cbUID, &
      szAuthStr,cbAuthStr) &
      bind(C, name="SQLConnectW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_short),intent(out) :: szDSN
      integer(kind=c_short),intent(in),value :: cbDSN
      integer(kind=c_short),intent(out) :: szUID
      integer(kind=c_short),intent(in),value :: cbUID
      integer(kind=c_short),intent(out) :: szAuthStr
      integer(kind=c_short),intent(in),value :: cbAuthStr
    end function SQLConnectW0
  end interface SQLConnectW

  interface SQLNumParams
    integer(kind=c_short) function SQLNumParams0 &
      (hstmt,pcpar) &
      bind(C, name="SQLNumParams")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: pcpar
    end function SQLNumParams0
  end interface SQLNumParams

  interface SQLDriverConnectW
    integer(kind=c_short) function SQLDriverConnectW0 &
      (hdbc,hwnd,szConnStrIn,cbConnStrIn,szConnStrOut, &
      cbConnStrOutMax,pcbConnStrOut,fDriverCompletion) &
      bind(C, name="SQLDriverConnectW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      type(c_ptr),intent(in),value :: hwnd
      integer(kind=c_short),intent(out) :: szConnStrIn
      integer(kind=c_short),intent(in),value :: cbConnStrIn
      integer(kind=c_short),intent(out) :: szConnStrOut
      integer(kind=c_short),intent(in),value :: cbConnStrOutMax
      integer(kind=c_short),intent(out) :: pcbConnStrOut
      integer(kind=c_short),intent(in),value :: fDriverCompletion
    end function SQLDriverConnectW0
  end interface SQLDriverConnectW

  interface SQLSpecialColumnsA
    integer(kind=c_short) function SQLSpecialColumnsA0 &
      (hstmt,fColType,szCatalogName,cbCatalogName,szSchemaName, &
      cbSchemaName,szTableName,cbTableName,fScope,fNullable) &
      bind(C, name="SQLSpecialColumnsA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: fColType
      character(kind=c_char) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      integer(kind=c_short),intent(in),value :: fScope
      integer(kind=c_short),intent(in),value :: fNullable
    end function SQLSpecialColumnsA0
  end interface SQLSpecialColumnsA

  interface SQLBindParameter
    integer(kind=c_short) function SQLBindParameter0 &
      (hstmt,ipar,fParamType,fCType,fSqlType, &
      cbColDef,ibScale,rgbValue,cbValueMax,pcbValue) &
      bind(C, name="SQLBindParameter")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: ipar
      integer(kind=c_short),intent(in),value :: fParamType
      integer(kind=c_short),intent(in),value :: fCType
      integer(kind=c_short),intent(in),value :: fSqlType
      integer(kind=c_long),intent(in),value :: cbColDef
      integer(kind=c_short),intent(in),value :: ibScale
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValueMax
      integer(kind=c_long),intent(out) :: pcbValue
    end function SQLBindParameter0
    module procedure SQLBindParameterInt
    module procedure SQLBindParameterInt_
    module procedure SQLBindParameterInt__
    module procedure SQLBindParameterReal
    module procedure SQLBindParameterReal_
    module procedure SQLBindParameterReal__
    module procedure SQLBindParameterDouble
    module procedure SQLBindParameterDouble_
    module procedure SQLBindParameterDouble__
    module procedure SQLBindParameterTimeStamp
    module procedure SQLBindParameterTimeStamp_
    module procedure SQLBindParameterTimeStamp__
    module procedure SQLBindParameterDate
    module procedure SQLBindParameterDate_
    module procedure SQLBindParameterDate__
  end interface SQLBindParameter

  interface SQLSpecialColumnsW
    integer(kind=c_short) function SQLSpecialColumnsW0 &
      (hstmt,fColType,szCatalogName,cbCatalogName,szSchemaName, &
      cbSchemaName,szTableName,cbTableName,fScope,fNullable) &
      bind(C, name="SQLSpecialColumnsW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: fColType
      integer(kind=c_short),intent(out) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      integer(kind=c_short),intent(out) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      integer(kind=c_short),intent(out) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      integer(kind=c_short),intent(in),value :: fScope
      integer(kind=c_short),intent(in),value :: fNullable
    end function SQLSpecialColumnsW0
  end interface SQLSpecialColumnsW

  interface SQLGetConnectAttrW
    integer(kind=c_short) function SQLGetConnectAttrW0 &
      (hdbc,fAttribute,rgbValue,cbValueMax,pcbValue) &
      bind(C, name="SQLGetConnectAttrW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_long),intent(in),value :: fAttribute
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValueMax
      integer(kind=c_long),intent(out) :: pcbValue
    end function SQLGetConnectAttrW0
  end interface SQLGetConnectAttrW

  interface SQLExecDirectW
    integer(kind=c_short) function SQLExecDirectW0 &
      (hstmt,szSqlStr,cbSqlStr) &
      bind(C, name="SQLExecDirectW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szSqlStr
      integer(kind=c_long),intent(in),value :: cbSqlStr
    end function SQLExecDirectW0
  end interface SQLExecDirectW

  interface SQLDisconnect
    integer(kind=c_short) function SQLDisconnect0 &
      (ConnectionHandle) &
      bind(C, name="SQLDisconnect")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
    end function SQLDisconnect0
  end interface SQLDisconnect

  interface SQLGetTypeInfo
    integer(kind=c_short) function SQLGetTypeInfo0 &
      (StatementHandle,DataType) &
      bind(C, name="SQLGetTypeInfo")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: DataType
    end function SQLGetTypeInfo0
  end interface SQLGetTypeInfo

  interface SQLStatistics
    integer(kind=c_short) function SQLStatistics0 &
      (StatementHandle,CatalogName,NameLength1,SchemaName,NameLength2, &
      TableName,NameLength3,Unique,Reserved) &
      bind(C, name="SQLStatistics")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      character(kind=c_char) :: CatalogName
      integer(kind=c_short),intent(in),value :: NameLength1
      character(kind=c_char) :: SchemaName
      integer(kind=c_short),intent(in),value :: NameLength2
      character(kind=c_char) :: TableName
      integer(kind=c_short),intent(in),value :: NameLength3
      integer(kind=c_short),intent(in),value :: Unique
      integer(kind=c_short),intent(in),value :: Reserved
    end function SQLStatistics0
  end interface SQLStatistics

  interface SQLGetDescField
    integer(kind=c_short) function SQLGetDescField0 &
      (DescriptorHandle,RecNumber,FieldIdentifier,Value,BufferLength, &
      StringLength) &
      bind(C, name="SQLGetDescField")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: DescriptorHandle
      integer(kind=c_short),intent(in),value :: RecNumber
      integer(kind=c_short),intent(in),value :: FieldIdentifier
      type(c_ptr),intent(in),value :: Value
      integer(kind=c_long),intent(in),value :: BufferLength
      integer(kind=c_long),intent(out) :: StringLength
    end function SQLGetDescField0
  end interface SQLGetDescField

  interface SQLSetCursorName
    integer(kind=c_short) function SQLSetCursorName0 &
      (StatementHandle,CursorName,NameLength) &
      bind(C, name="SQLSetCursorName")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      character(kind=c_char) :: CursorName
      integer(kind=c_short),intent(in),value :: NameLength
    end function SQLSetCursorName0
  end interface SQLSetCursorName

  interface SQLPrimaryKeysA
    integer(kind=c_short) function SQLPrimaryKeysA0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName) &
      bind(C, name="SQLPrimaryKeysA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
    end function SQLPrimaryKeysA0
  end interface SQLPrimaryKeysA

  interface SQLDescribeColW
    integer(kind=c_short) function SQLDescribeColW0 &
      (hstmt,icol,szColName,cbColNameMax,pcbColName, &
      pfSqlType,pcbColDef,pibScale,pfNullable) &
      bind(C, name="SQLDescribeColW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: icol
      integer(kind=c_short),intent(out) :: szColName
      integer(kind=c_short),intent(in),value :: cbColNameMax
      integer(kind=c_short),intent(out) :: pcbColName
      integer(kind=c_short),intent(out) :: pfSqlType
      integer(kind=c_long),intent(out) :: pcbColDef
      integer(kind=c_short),intent(out) :: pibScale
      integer(kind=c_short),intent(out) :: pfNullable
    end function SQLDescribeColW0
  end interface SQLDescribeColW

  interface SQLAllocConnect
    integer(kind=c_short) function SQLAllocConnect0 &
      (EnvironmentHandle,ConnectionHandle) &
      bind(C, name="SQLAllocConnect")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: EnvironmentHandle
      type(c_ptr),intent(out) :: ConnectionHandle
    end function SQLAllocConnect0
  end interface SQLAllocConnect

  interface SQLDriverConnect
    integer(kind=c_short) function SQLDriverConnect0 &
      (hdbc,hwnd,szConnStrIn,cbConnStrIn,szConnStrOut, &
      cbConnStrOutMax,pcbConnStrOut,fDriverCompletion) &
      bind(C, name="SQLDriverConnect")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      type(c_ptr),intent(in),value :: hwnd
      character(kind=c_char) :: szConnStrIn
      integer(kind=c_short),intent(in),value :: cbConnStrIn
      character(kind=c_char) :: szConnStrOut
      integer(kind=c_short),intent(in),value :: cbConnStrOutMax
      integer(kind=c_short),intent(out) :: pcbConnStrOut
      integer(kind=c_short),intent(in),value :: fDriverCompletion
    end function SQLDriverConnect0
  end interface SQLDriverConnect

  interface ODBCSetTryWaitValue
    integer(kind=c_int) function ODBCSetTryWaitValue0 &
      (dwValue) &
      bind(C, name="ODBCSetTryWaitValue")
      use, intrinsic :: iso_c_binding
      integer(kind=c_long),intent(in),value :: dwValue
    end function ODBCSetTryWaitValue0
  end interface ODBCSetTryWaitValue

  interface SQLProcedureColumnsW
    integer(kind=c_short) function SQLProcedureColumnsW0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szProcName,cbProcName,szColumnName,cbColumnName) &
      bind(C, name="SQLProcedureColumnsW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      integer(kind=c_short),intent(out) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      integer(kind=c_short),intent(out) :: szProcName
      integer(kind=c_short),intent(in),value :: cbProcName
      integer(kind=c_short),intent(out) :: szColumnName
      integer(kind=c_short),intent(in),value :: cbColumnName
    end function SQLProcedureColumnsW0
  end interface SQLProcedureColumnsW

  interface SQLCopyDesc
    integer(kind=c_short) function SQLCopyDesc0 &
      (SourceDescHandle,TargetDescHandle) &
      bind(C, name="SQLCopyDesc")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: SourceDescHandle
      type(c_ptr),intent(in),value :: TargetDescHandle
    end function SQLCopyDesc0
  end interface SQLCopyDesc

  interface SQLNativeSqlA
    integer(kind=c_short) function SQLNativeSqlA0 &
      (hdbc,szSqlStrIn,cbSqlStrIn,szSqlStr,cbSqlStrMax, &
      pcbSqlStr) &
      bind(C, name="SQLNativeSqlA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      character(kind=c_char) :: szSqlStrIn
      integer(kind=c_long),intent(in),value :: cbSqlStrIn
      character(kind=c_char) :: szSqlStr
      integer(kind=c_long),intent(in),value :: cbSqlStrMax
      integer(kind=c_long),intent(out) :: pcbSqlStr
    end function SQLNativeSqlA0
  end interface SQLNativeSqlA

  interface SQLColAttributesA
    integer(kind=c_short) function SQLColAttributesA0 &
      (hstmt,icol,fDescType,rgbDesc,cbDescMax, &
      pcbDesc,pfDesc) &
      bind(C, name="SQLColAttributesA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: icol
      integer(kind=c_short),intent(in),value :: fDescType
      type(c_ptr),intent(in),value :: rgbDesc
      integer(kind=c_short),intent(in),value :: cbDescMax
      integer(kind=c_short),intent(out) :: pcbDesc
      integer(kind=c_long),intent(out) :: pfDesc
    end function SQLColAttributesA0
  end interface SQLColAttributesA

  interface SQLSetDescRec
    integer(kind=c_short) function SQLSetDescRec0 &
      (DescriptorHandle,RecNumber,Type,SubType,Length, &
      Precision,Scale,Data,StringLength,Indicator) &
      bind(C, name="SQLSetDescRec")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: DescriptorHandle
      integer(kind=c_short),intent(in),value :: RecNumber
      integer(kind=c_short),intent(in),value :: Type
      integer(kind=c_short),intent(in),value :: SubType
      integer(kind=c_long),intent(in),value :: Length
      integer(kind=c_short),intent(in),value :: Precision
      integer(kind=c_short),intent(in),value :: Scale
      type(c_ptr),intent(in),value :: Data
      integer(kind=c_long),intent(out) :: StringLength
      integer(kind=c_long),intent(out) :: Indicator
    end function SQLSetDescRec0
  end interface SQLSetDescRec

  interface SQLProcedures
    integer(kind=c_short) function SQLProcedures0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szProcName,cbProcName) &
      bind(C, name="SQLProcedures")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char) :: szProcName
      integer(kind=c_short),intent(in),value :: cbProcName
    end function SQLProcedures0
  end interface SQLProcedures

  interface SQLPutData
    integer(kind=c_short) function SQLPutData0 &
      (StatementHandle,Data,StrLen_or_Ind) &
      bind(C, name="SQLPutData")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      type(c_ptr),intent(in),value :: Data
      integer(kind=c_long),intent(in),value :: StrLen_or_Ind
    end function SQLPutData0
  end interface SQLPutData

  interface SQLColumnsA
    integer(kind=c_short) function SQLColumnsA0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName,szColumnName,cbColumnName) &
      bind(C, name="SQLColumnsA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      character(kind=c_char) :: szColumnName
      integer(kind=c_short),intent(in),value :: cbColumnName
    end function SQLColumnsA0
  end interface SQLColumnsA

  interface SQLColumnsW
    integer(kind=c_short) function SQLColumnsW0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName,szColumnName,cbColumnName) &
      bind(C, name="SQLColumnsW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      integer(kind=c_short),intent(out) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      integer(kind=c_short),intent(out) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      integer(kind=c_short),intent(out) :: szColumnName
      integer(kind=c_short),intent(in),value :: cbColumnName
    end function SQLColumnsW0
  end interface SQLColumnsW

  interface SQLSetConnectAttrW
    integer(kind=c_short) function SQLSetConnectAttrW0 &
      (hdbc,fAttribute,rgbValue,cbValue) &
      bind(C, name="SQLSetConnectAttrW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_long),intent(in),value :: fAttribute
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValue
    end function SQLSetConnectAttrW0
  end interface SQLSetConnectAttrW

  interface SQLGetEnvAttr
    integer(kind=c_short) function SQLGetEnvAttr0 &
      (EnvironmentHandle,Attribute,Value,BufferLength,StringLength) &
      bind(C, name="SQLGetEnvAttr")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: EnvironmentHandle
      integer(kind=c_long),intent(in),value :: Attribute
      type(c_ptr),intent(in),value :: Value
      integer(kind=c_long),intent(in),value :: BufferLength
      integer(kind=c_long),intent(out) :: StringLength
    end function SQLGetEnvAttr0
  end interface SQLGetEnvAttr

  interface SQLBrowseConnectW
    integer(kind=c_short) function SQLBrowseConnectW0 &
      (hdbc,szConnStrIn,cbConnStrIn,szConnStrOut,cbConnStrOutMax, &
      pcbConnStrOut) &
      bind(C, name="SQLBrowseConnectW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_short),intent(out) :: szConnStrIn
      integer(kind=c_short),intent(in),value :: cbConnStrIn
      integer(kind=c_short),intent(out) :: szConnStrOut
      integer(kind=c_short),intent(in),value :: cbConnStrOutMax
      integer(kind=c_short),intent(out) :: pcbConnStrOut
    end function SQLBrowseConnectW0
  end interface SQLBrowseConnectW

  interface SQLColAttributesW
    integer(kind=c_short) function SQLColAttributesW0 &
      (hstmt,icol,fDescType,rgbDesc,cbDescMax, &
      pcbDesc,pfDesc) &
      bind(C, name="SQLColAttributesW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: icol
      integer(kind=c_short),intent(in),value :: fDescType
      type(c_ptr),intent(in),value :: rgbDesc
      integer(kind=c_short),intent(in),value :: cbDescMax
      integer(kind=c_short),intent(out) :: pcbDesc
      integer(kind=c_long),intent(out) :: pfDesc
    end function SQLColAttributesW0
  end interface SQLColAttributesW

  interface SQLExecDirect
    integer(kind=c_short) function SQLExecDirect0 &
      (StatementHandle,StatementText,TextLength) &
      bind(C, name="SQLExecDirect")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      character(kind=c_char) :: StatementText
      integer(kind=c_long),intent(in),value :: TextLength
    end function SQLExecDirect0
  end interface SQLExecDirect

  interface SQLGetCursorNameW
    integer(kind=c_short) function SQLGetCursorNameW0 &
      (hstmt,szCursor,cbCursorMax,pcbCursor) &
      bind(C, name="SQLGetCursorNameW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCursor
      integer(kind=c_short),intent(in),value :: cbCursorMax
      integer(kind=c_short),intent(out) :: pcbCursor
    end function SQLGetCursorNameW0
  end interface SQLGetCursorNameW

  interface SQLColumnPrivileges
    integer(kind=c_short) function SQLColumnPrivileges0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName,szColumnName,cbColumnName) &
      bind(C, name="SQLColumnPrivileges")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      character(kind=c_char) :: szColumnName
      integer(kind=c_short),intent(in),value :: cbColumnName
    end function SQLColumnPrivileges0
  end interface SQLColumnPrivileges

  interface SQLColumnPrivilegesA
    integer(kind=c_short) function SQLColumnPrivilegesA0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName,szColumnName,cbColumnName) &
      bind(C, name="SQLColumnPrivilegesA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      character(kind=c_char) :: szColumnName
      integer(kind=c_short),intent(in),value :: cbColumnName
    end function SQLColumnPrivilegesA0
  end interface SQLColumnPrivilegesA

  interface SQLColumnPrivilegesW
    integer(kind=c_short) function SQLColumnPrivilegesW0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName,szColumnName,cbColumnName) &
      bind(C, name="SQLColumnPrivilegesW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      integer(kind=c_short),intent(out) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      integer(kind=c_short),intent(out) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      integer(kind=c_short),intent(out) :: szColumnName
      integer(kind=c_short),intent(in),value :: cbColumnName
    end function SQLColumnPrivilegesW0
  end interface SQLColumnPrivilegesW

  interface SQLGetConnectOptionA
    integer(kind=c_short) function SQLGetConnectOptionA0 &
      (hdbc,fOption,pvParam) &
      bind(C, name="SQLGetConnectOptionA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_short),intent(in),value :: fOption
      type(c_ptr),intent(in),value :: pvParam
    end function SQLGetConnectOptionA0
  end interface SQLGetConnectOptionA

  interface TraceReturn
    integer(kind=c_short) function TraceReturn0 &
      (var1,var2) &
      bind(C, name="TraceReturn")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: var1
      integer(kind=c_short),intent(in),value :: var2
    end function TraceReturn0
  end interface TraceReturn

  interface SQLGetDiagRecA
    integer(kind=c_short) function SQLGetDiagRecA0 &
      (fHandleType,handle,iRecord,szSqlState,pfNativeError, &
      szErrorMsg,cbErrorMsgMax,pcbErrorMsg) &
      bind(C, name="SQLGetDiagRecA")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: fHandleType
      type(c_ptr),intent(in),value :: handle
      integer(kind=c_short),intent(in),value :: iRecord
      character(kind=c_char) :: szSqlState
      integer(kind=c_long),intent(out) :: pfNativeError
      character(kind=c_char) :: szErrorMsg
      integer(kind=c_short),intent(in),value :: cbErrorMsgMax
      integer(kind=c_short),intent(out) :: pcbErrorMsg
    end function SQLGetDiagRecA0
  end interface SQLGetDiagRecA

  interface SQLGetDiagRecW
    integer(kind=c_short) function SQLGetDiagRecW0 &
      (fHandleType,handle,iRecord,szSqlState,pfNativeError, &
      szErrorMsg,cbErrorMsgMax,pcbErrorMsg) &
      bind(C, name="SQLGetDiagRecW")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: fHandleType
      type(c_ptr),intent(in),value :: handle
      integer(kind=c_short),intent(in),value :: iRecord
      integer(kind=c_short),intent(out) :: szSqlState
      integer(kind=c_long),intent(out) :: pfNativeError
      integer(kind=c_short),intent(out) :: szErrorMsg
      integer(kind=c_short),intent(in),value :: cbErrorMsgMax
      integer(kind=c_short),intent(out) :: pcbErrorMsg
    end function SQLGetDiagRecW0
  end interface SQLGetDiagRecW

  interface SQLPrimaryKeys
    integer(kind=c_short) function SQLPrimaryKeys0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName) &
      bind(C, name="SQLPrimaryKeys")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
    end function SQLPrimaryKeys0
  end interface SQLPrimaryKeys

  interface SQLSetConnectOptionA
    integer(kind=c_short) function SQLSetConnectOptionA0 &
      (hdbc,fOption,vParam) &
      bind(C, name="SQLSetConnectOptionA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_short),intent(in),value :: fOption
      integer(kind=c_long),intent(in),value :: vParam
    end function SQLSetConnectOptionA0
  end interface SQLSetConnectOptionA

  interface SQLForeignKeys
    integer(kind=c_short) function SQLForeignKeys0 &
      (hstmt,szPkCatalogName,cbPkCatalogName,szPkSchemaName,cbPkSchemaName, &
      szPkTableName,cbPkTableName,szFkCatalogName,cbFkCatalogName,szFkSchemaName, &
      cbFkSchemaName,szFkTableName,cbFkTableName) &
      bind(C, name="SQLForeignKeys")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szPkCatalogName
      integer(kind=c_short),intent(in),value :: cbPkCatalogName
      character(kind=c_char) :: szPkSchemaName
      integer(kind=c_short),intent(in),value :: cbPkSchemaName
      character(kind=c_char) :: szPkTableName
      integer(kind=c_short),intent(in),value :: cbPkTableName
      character(kind=c_char) :: szFkCatalogName
      integer(kind=c_short),intent(in),value :: cbFkCatalogName
      character(kind=c_char) :: szFkSchemaName
      integer(kind=c_short),intent(in),value :: cbFkSchemaName
      character(kind=c_char) :: szFkTableName
      integer(kind=c_short),intent(in),value :: cbFkTableName
    end function SQLForeignKeys0
  end interface SQLForeignKeys

  interface SQLSetConnectAttrA
    integer(kind=c_short) function SQLSetConnectAttrA0 &
      (hdbc,fAttribute,rgbValue,cbValue) &
      bind(C, name="SQLSetConnectAttrA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_long),intent(in),value :: fAttribute
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValue
    end function SQLSetConnectAttrA0
  end interface SQLSetConnectAttrA

  interface SQLDriversA
    integer(kind=c_short) function SQLDriversA0 &
      (henv,fDirection,szDriverDesc,cbDriverDescMax,pcbDriverDesc, &
      szDriverAttributes,cbDrvrAttrMax,pcbDrvrAttr) &
      bind(C, name="SQLDriversA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: henv
      integer(kind=c_short),intent(in),value :: fDirection
      character(kind=c_char) :: szDriverDesc
      integer(kind=c_short),intent(in),value :: cbDriverDescMax
      integer(kind=c_short),intent(out) :: pcbDriverDesc
      character(kind=c_char) :: szDriverAttributes
      integer(kind=c_short),intent(in),value :: cbDrvrAttrMax
      integer(kind=c_short),intent(out) :: pcbDrvrAttr
    end function SQLDriversA0
  end interface SQLDriversA

  interface SQLColAttributeA
    integer(kind=c_short) function SQLColAttributeA0 &
      (hstmt,iCol,iField,pCharAttr,cbCharAttrMax, &
      pcbCharAttr,pNumAttr) &
      bind(C, name="SQLColAttributeA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: iCol
      integer(kind=c_short),intent(in),value :: iField
      type(c_ptr),intent(in),value :: pCharAttr
      integer(kind=c_short),intent(in),value :: cbCharAttrMax
      integer(kind=c_short),intent(out) :: pcbCharAttr
      integer(kind=c_long),intent(out) :: pNumAttr
    end function SQLColAttributeA0
  end interface SQLColAttributeA

  interface SQLDriversW
    integer(kind=c_short) function SQLDriversW0 &
      (henv,fDirection,szDriverDesc,cbDriverDescMax,pcbDriverDesc, &
      szDriverAttributes,cbDrvrAttrMax,pcbDrvrAttr) &
      bind(C, name="SQLDriversW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: henv
      integer(kind=c_short),intent(in),value :: fDirection
      integer(kind=c_short),intent(out) :: szDriverDesc
      integer(kind=c_short),intent(in),value :: cbDriverDescMax
      integer(kind=c_short),intent(out) :: pcbDriverDesc
      integer(kind=c_short),intent(out) :: szDriverAttributes
      integer(kind=c_short),intent(in),value :: cbDrvrAttrMax
      integer(kind=c_short),intent(out) :: pcbDrvrAttr
    end function SQLDriversW0
  end interface SQLDriversW

  interface SQLProcedureColumns
    integer(kind=c_short) function SQLProcedureColumns0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szProcName,cbProcName,szColumnName,cbColumnName) &
      bind(C, name="SQLProcedureColumns")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char) :: szProcName
      integer(kind=c_short),intent(in),value :: cbProcName
      character(kind=c_char) :: szColumnName
      integer(kind=c_short),intent(in),value :: cbColumnName
    end function SQLProcedureColumns0
  end interface SQLProcedureColumns

  interface SQLFetch
    integer(kind=c_short) function SQLFetch0 &
      (StatementHandle) &
      bind(C, name="SQLFetch")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
    end function SQLFetch0
  end interface SQLFetch

  interface SQLSetStmtOption
    integer(kind=c_short) function SQLSetStmtOption0 &
      (StatementHandle,Option,Value) &
      bind(C, name="SQLSetStmtOption")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: Option
      integer(kind=c_long),intent(in),value :: Value
    end function SQLSetStmtOption0
  end interface SQLSetStmtOption

  interface SQLProceduresA
    integer(kind=c_short) function SQLProceduresA0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szProcName,cbProcName) &
      bind(C, name="SQLProceduresA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char) :: szProcName
      integer(kind=c_short),intent(in),value :: cbProcName
    end function SQLProceduresA0
  end interface SQLProceduresA

  interface SQLGetDescRecA
    integer(kind=c_short) function SQLGetDescRecA0 &
      (hdesc,iRecord,szName,cbNameMax,pcbName, &
      pfType,pfSubType,pLength,pPrecision,pScale, &
      pNullable) &
      bind(C, name="SQLGetDescRecA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdesc
      integer(kind=c_short),intent(in),value :: iRecord
      character(kind=c_char) :: szName
      integer(kind=c_short),intent(in),value :: cbNameMax
      integer(kind=c_short),intent(out) :: pcbName
      integer(kind=c_short),intent(out) :: pfType
      integer(kind=c_short),intent(out) :: pfSubType
      integer(kind=c_long),intent(out) :: pLength
      integer(kind=c_short),intent(out) :: pPrecision
      integer(kind=c_short),intent(out) :: pScale
      integer(kind=c_short),intent(out) :: pNullable
    end function SQLGetDescRecA0
  end interface SQLGetDescRecA

  interface SQLGetDescRecW
    integer(kind=c_short) function SQLGetDescRecW0 &
      (hdesc,iRecord,szName,cbNameMax,pcbName, &
      pfType,pfSubType,pLength,pPrecision,pScale, &
      pNullable) &
      bind(C, name="SQLGetDescRecW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdesc
      integer(kind=c_short),intent(in),value :: iRecord
      integer(kind=c_short),intent(out) :: szName
      integer(kind=c_short),intent(in),value :: cbNameMax
      integer(kind=c_short),intent(out) :: pcbName
      integer(kind=c_short),intent(out) :: pfType
      integer(kind=c_short),intent(out) :: pfSubType
      integer(kind=c_long),intent(out) :: pLength
      integer(kind=c_short),intent(out) :: pPrecision
      integer(kind=c_short),intent(out) :: pScale
      integer(kind=c_short),intent(out) :: pNullable
    end function SQLGetDescRecW0
  end interface SQLGetDescRecW

  interface TraceCloseLogFile
    integer(kind=c_short) function TraceCloseLogFile0 &
      (var1) &
      bind(C, name="TraceCloseLogFile")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: var1
    end function TraceCloseLogFile0
  end interface TraceCloseLogFile

  interface SQLSetCursorNameA
    integer(kind=c_short) function SQLSetCursorNameA0 &
      (hstmt,szCursor,cbCursor) &
      bind(C, name="SQLSetCursorNameA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szCursor
      integer(kind=c_short),intent(in),value :: cbCursor
    end function SQLSetCursorNameA0
  end interface SQLSetCursorNameA

  interface SQLGetStmtOptionA
    integer(kind=c_short) function SQLGetStmtOptionA0 &
      (hstmt,fOption,pvParam) &
      bind(C, name="SQLGetStmtOptionA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: fOption
      type(c_ptr),intent(in),value :: pvParam
    end function SQLGetStmtOptionA0
  end interface SQLGetStmtOptionA

  interface SQLErrorA
    integer(kind=c_short) function SQLErrorA0 &
      (henv,hdbc,hstmt,szSqlState,pfNativeError, &
      szErrorMsg,cbErrorMsgMax,pcbErrorMsg) &
      bind(C, name="SQLErrorA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: henv
      type(c_ptr),intent(in),value :: hdbc
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szSqlState
      integer(kind=c_long),intent(out) :: pfNativeError
      character(kind=c_char) :: szErrorMsg
      integer(kind=c_short),intent(in),value :: cbErrorMsgMax
      integer(kind=c_short),intent(out) :: pcbErrorMsg
    end function SQLErrorA0
  end interface SQLErrorA

  interface SQLErrorW
    integer(kind=c_short) function SQLErrorW0 &
      (henv,hdbc,hstmt,szSqlState,pfNativeError, &
      szErrorMsg,cbErrorMsgMax,pcbErrorMsg) &
      bind(C, name="SQLErrorW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: henv
      type(c_ptr),intent(in),value :: hdbc
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szSqlState
      integer(kind=c_long),intent(out) :: pfNativeError
      integer(kind=c_short),intent(out) :: szErrorMsg
      integer(kind=c_short),intent(in),value :: cbErrorMsgMax
      integer(kind=c_short),intent(out) :: pcbErrorMsg
    end function SQLErrorW0
  end interface SQLErrorW

  interface SQLSetCursorNameW
    integer(kind=c_short) function SQLSetCursorNameW0 &
      (hstmt,szCursor,cbCursor) &
      bind(C, name="SQLSetCursorNameW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCursor
      integer(kind=c_short),intent(in),value :: cbCursor
    end function SQLSetCursorNameW0
  end interface SQLSetCursorNameW

  interface SQLProceduresW
    integer(kind=c_short) function SQLProceduresW0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szProcName,cbProcName) &
      bind(C, name="SQLProceduresW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      integer(kind=c_short),intent(out) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      integer(kind=c_short),intent(out) :: szProcName
      integer(kind=c_short),intent(in),value :: cbProcName
    end function SQLProceduresW0
  end interface SQLProceduresW

  interface SQLDescribeParam
    integer(kind=c_short) function SQLDescribeParam0 &
      (hstmt,ipar,pfSqlType,pcbParamDef,pibScale, &
      pfNullable) &
      bind(C, name="SQLDescribeParam")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: ipar
      integer(kind=c_short),intent(out) :: pfSqlType
      integer(kind=c_long),intent(out) :: pcbParamDef
      integer(kind=c_short),intent(out) :: pibScale
      integer(kind=c_short),intent(out) :: pfNullable
    end function SQLDescribeParam0
  end interface SQLDescribeParam

  interface SQLColAttribute
    integer(kind=c_short) function SQLColAttribute0 &
      (StatementHandle,ColumnNumber,FieldIdentifier,CharacterAttribute,BufferLength, &
      StringLength,NumericAttribute) &
      bind(C, name="SQLColAttribute")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: ColumnNumber
      integer(kind=c_short),intent(in),value :: FieldIdentifier
      type(c_ptr),intent(in),value :: CharacterAttribute
      integer(kind=c_short),intent(in),value :: BufferLength
      integer(kind=c_short),intent(out) :: StringLength
      integer(kind=c_long),intent(out) :: NumericAttribute
    end function SQLColAttribute0
  end interface SQLColAttribute

  interface SQLAllocHandleStd
    integer(kind=c_short) function SQLAllocHandleStd0 &
      (fHandleType,hInput,phOutput) &
      bind(C, name="SQLAllocHandleStd")
      use, intrinsic :: iso_c_binding
      integer(kind=c_short),intent(in),value :: fHandleType
      type(c_ptr),intent(in),value :: hInput
      type(c_ptr),intent(out) :: phOutput
    end function SQLAllocHandleStd0
  end interface SQLAllocHandleStd

  interface SQLSetConnectOption
    integer(kind=c_short) function SQLSetConnectOption0 &
      (ConnectionHandle,Option,Value) &
      bind(C, name="SQLSetConnectOption")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
      integer(kind=c_short),intent(in),value :: Option
      integer(kind=c_long),intent(in),value :: Value
    end function SQLSetConnectOption0
  end interface SQLSetConnectOption

  interface SQLMoreResults
    integer(kind=c_short) function SQLMoreResults0 &
      (hstmt) &
      bind(C, name="SQLMoreResults")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
    end function SQLMoreResults0
  end interface SQLMoreResults

  interface SQLGetData
    integer(kind=c_short) function SQLGetData0 &
      (StatementHandle,ColumnNumber,TargetType,TargetValue,BufferLength, &
      StrLen_or_Ind) &
      bind(C, name="SQLGetData")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: ColumnNumber
      integer(kind=c_short),intent(in),value :: TargetType
      type(c_ptr),intent(in),value :: TargetValue
      integer(kind=c_long),intent(in),value :: BufferLength
      integer(kind=c_long),intent(out) :: StrLen_or_Ind
    end function SQLGetData0
    module procedure SQLGetDataInt
    module procedure SQLGetDataReal
    module procedure SQLGetDataDouble
    module procedure SQLGetDataTimeStamp
    module procedure SQLGetDataDate
  end interface SQLGetData

  interface SQLTablesW
    integer(kind=c_short) function SQLTablesW0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName,szTableType,cbTableType) &
      bind(C, name="SQLTablesW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      integer(kind=c_short),intent(out) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      integer(kind=c_short),intent(out) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      integer(kind=c_short),intent(out) :: szTableType
      integer(kind=c_short),intent(in),value :: cbTableType
    end function SQLTablesW0
  end interface SQLTablesW

  interface SQLCancel
    integer(kind=c_short) function SQLCancel0 &
      (StatementHandle) &
      bind(C, name="SQLCancel")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
    end function SQLCancel0
  end interface SQLCancel

  interface SQLStatisticsA
    integer(kind=c_short) function SQLStatisticsA0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName,fUnique,fAccuracy) &
      bind(C, name="SQLStatisticsA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      character(kind=c_char) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      character(kind=c_char) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      character(kind=c_char) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      integer(kind=c_short),intent(in),value :: fUnique
      integer(kind=c_short),intent(in),value :: fAccuracy
    end function SQLStatisticsA0
  end interface SQLStatisticsA

  interface SQLStatisticsW
    integer(kind=c_short) function SQLStatisticsW0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName,fUnique,fAccuracy) &
      bind(C, name="SQLStatisticsW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      integer(kind=c_short),intent(out) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      integer(kind=c_short),intent(out) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
      integer(kind=c_short),intent(in),value :: fUnique
      integer(kind=c_short),intent(in),value :: fAccuracy
    end function SQLStatisticsW0
  end interface SQLStatisticsW

  interface SQLDescribeParamA
    integer(kind=c_short) function SQLDescribeParamA0 &
      (hstmt,ipar,pfSqlType,pcbParamDef,pibScale, &
      pfNullable) &
      bind(C, name="SQLDescribeParamA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(in),value :: ipar
      integer(kind=c_short),intent(out) :: pfSqlType
      integer(kind=c_long),intent(out) :: pcbParamDef
      integer(kind=c_short),intent(out) :: pibScale
      integer(kind=c_short),intent(out) :: pfNullable
    end function SQLDescribeParamA0
  end interface SQLDescribeParamA

  interface SQLGetDescFieldA
    integer(kind=c_short) function SQLGetDescFieldA0 &
      (hdesc,iRecord,iField,rgbValue,cbValueMax, &
      pcbValue) &
      bind(C, name="SQLGetDescFieldA")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdesc
      integer(kind=c_short),intent(in),value :: iRecord
      integer(kind=c_short),intent(in),value :: iField
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValueMax
      integer(kind=c_long),intent(out) :: pcbValue
    end function SQLGetDescFieldA0
  end interface SQLGetDescFieldA

  interface SQLPrimaryKeysW
    integer(kind=c_short) function SQLPrimaryKeysW0 &
      (hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName, &
      szTableName,cbTableName) &
      bind(C, name="SQLPrimaryKeysW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hstmt
      integer(kind=c_short),intent(out) :: szCatalogName
      integer(kind=c_short),intent(in),value :: cbCatalogName
      integer(kind=c_short),intent(out) :: szSchemaName
      integer(kind=c_short),intent(in),value :: cbSchemaName
      integer(kind=c_short),intent(out) :: szTableName
      integer(kind=c_short),intent(in),value :: cbTableName
    end function SQLPrimaryKeysW0
  end interface SQLPrimaryKeysW

  interface SQLGetDescFieldW
    integer(kind=c_short) function SQLGetDescFieldW0 &
      (hdesc,iRecord,iField,rgbValue,cbValueMax, &
      pcbValue) &
      bind(C, name="SQLGetDescFieldW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdesc
      integer(kind=c_short),intent(in),value :: iRecord
      integer(kind=c_short),intent(in),value :: iField
      type(c_ptr),intent(in),value :: rgbValue
      integer(kind=c_long),intent(in),value :: cbValueMax
      integer(kind=c_long),intent(out) :: pcbValue
    end function SQLGetDescFieldW0
  end interface SQLGetDescFieldW

  interface SQLNativeSqlW
    integer(kind=c_short) function SQLNativeSqlW0 &
      (hdbc,szSqlStrIn,cbSqlStrIn,szSqlStr,cbSqlStrMax, &
      pcbSqlStr) &
      bind(C, name="SQLNativeSqlW")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: hdbc
      integer(kind=c_short),intent(out) :: szSqlStrIn
      integer(kind=c_long),intent(in),value :: cbSqlStrIn
      integer(kind=c_short),intent(out) :: szSqlStr
      integer(kind=c_long),intent(in),value :: cbSqlStrMax
      integer(kind=c_long),intent(out) :: pcbSqlStr
    end function SQLNativeSqlW0
  end interface SQLNativeSqlW

  interface SQLTables
    integer(kind=c_short) function SQLTables0 &
      (StatementHandle,CatalogName,NameLength1,SchemaName,NameLength2, &
      TableName,NameLength3,TableType,NameLength4) &
      bind(C, name="SQLTables")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      character(kind=c_char) :: CatalogName
      integer(kind=c_short),intent(in),value :: NameLength1
      character(kind=c_char) :: SchemaName
      integer(kind=c_short),intent(in),value :: NameLength2
      character(kind=c_char) :: TableName
      integer(kind=c_short),intent(in),value :: NameLength3
      character(kind=c_char) :: TableType
      integer(kind=c_short),intent(in),value :: NameLength4
    end function SQLTables0
  end interface SQLTables

  interface SQLBindParam
    integer(kind=c_short) function SQLBindParam0 &
      (StatementHandle,ParameterNumber,ValueType,ParameterType,LengthPrecision, &
      ParameterScale,ParameterValue,StrLen_or_Ind) &
      bind(C, name="SQLBindParam")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: StatementHandle
      integer(kind=c_short),intent(in),value :: ParameterNumber
      integer(kind=c_short),intent(in),value :: ValueType
      integer(kind=c_short),intent(in),value :: ParameterType
      integer(kind=c_long),intent(in),value :: LengthPrecision
      integer(kind=c_short),intent(in),value :: ParameterScale
      type(c_ptr),intent(in),value :: ParameterValue
      integer(kind=c_long),intent(out) :: StrLen_or_Ind
    end function SQLBindParam0
  end interface SQLBindParam

  interface SQLAllocStmt
    integer(kind=c_short) function SQLAllocStmt0 &
      (ConnectionHandle,StatementHandle) &
      bind(C, name="SQLAllocStmt")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: ConnectionHandle
      type(c_ptr),intent(out) :: StatementHandle
    end function SQLAllocStmt0
  end interface SQLAllocStmt

  interface SQLTransact
    integer(kind=c_short) function SQLTransact0 &
      (EnvironmentHandle,ConnectionHandle,CompletionType) &
      bind(C, name="SQLTransact")
      use, intrinsic :: iso_c_binding
      type(c_ptr),intent(in),value :: EnvironmentHandle
      type(c_ptr),intent(in),value :: ConnectionHandle
      integer(kind=c_short),intent(in),value :: CompletionType
    end function SQLTransact0
  end interface SQLTransact

contains

  function SQLBindColInt &
       (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    integer(kind=c_int),target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLBindCol0(StatementHandle,ColumnNumber,SQL_INTEGER, &
        c_loc(TargetValue),c_sizeof(TargetValue), StrLen_or_Ind)
  end function SQLBindColInt

  function SQLGetDataInt &
        (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    integer(kind=c_int),target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLGetData0(StatementHandle,ColumnNumber,SQL_INTEGER, &
        c_loc(TargetValue),c_sizeof(TargetValue),StrLen_or_Ind)
  end function SQLGetDataInt

  function SQLBindParameterInt &
    (hstmt,ipar,fParamType,fSqlType, &
    cbColDef,ibScale,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    integer(kind=c_short),intent(in),value :: fSqlType
    integer(kind=c_long),intent(in),value :: cbColDef
    integer(kind=c_short),intent(in),value :: ibScale
    integer(kind=c_int),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,SQL_INTEGER,fSqlType, &
    cbColDef,ibScale,c_loc(rgbValue),c_sizeof(rgbValue),pcbValue)
  end function SQLBindParameterInt

  function SQLBindParameterInt_ &
    (hstmt,ipar,fParamType,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    integer(kind=c_int),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,SQL_INTEGER, &
    SQL_INTEGER, 0, 0_2,c_loc(rgbValue),c_sizeof(rgbValue),pcbValue)
  end function SQLBindParameterInt_

  function SQLBindParameterInt__ &
    (hstmt,ipar,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_int),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,SQL_PARAM_INPUT,SQL_INTEGER, &
    SQL_INTEGER, 0, 0_2,c_loc(rgbValue),c_sizeof(rgbValue),pcbValue)
  end function SQLBindParameterInt__

  function SQLBindColReal &
       (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    real(kind=c_float),target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLBindCol0(StatementHandle,ColumnNumber,SQL_REAL, &
        c_loc(TargetValue),c_sizeof(TargetValue), StrLen_or_Ind)
  end function SQLBindColReal

  function SQLGetDataReal &
        (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    real(kind=c_float),target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLGetData0(StatementHandle,ColumnNumber,SQL_REAL, &
        c_loc(TargetValue),c_sizeof(TargetValue),StrLen_or_Ind)
  end function SQLGetDataReal

  function SQLBindParameterReal &
    (hstmt,ipar,fParamType,fSqlType, &
    cbColDef,ibScale,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    integer(kind=c_short),intent(in),value :: fSqlType
    integer(kind=c_long),intent(in),value :: cbColDef
    integer(kind=c_short),intent(in),value :: ibScale
    real(kind=c_float),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,SQL_REAL,fSqlType, &
    cbColDef,ibScale,c_loc(rgbValue),c_sizeof(rgbValue),pcbValue)
  end function SQLBindParameterReal

  function SQLBindParameterReal_ &
    (hstmt,ipar,fParamType,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    real(kind=c_float),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,SQL_REAL, &
    SQL_REAL, 0, 0_2,c_loc(rgbValue),c_sizeof(rgbValue),pcbValue)
  end function SQLBindParameterReal_

  function SQLBindParameterReal__ &
    (hstmt,ipar,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    real(kind=c_float),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,SQL_PARAM_INPUT,SQL_REAL, &
    SQL_REAL, 0, 0_2,c_loc(rgbValue),c_sizeof(rgbValue),pcbValue)
  end function SQLBindParameterReal__

  function SQLBindColDouble &
       (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    real(kind=c_double),target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLBindCol0(StatementHandle,ColumnNumber,SQL_DOUBLE, &
        c_loc(TargetValue),c_sizeof(TargetValue), StrLen_or_Ind)
  end function SQLBindColDouble

  function SQLGetDataDouble &
        (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    real(kind=c_double),target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLGetData0(StatementHandle,ColumnNumber,SQL_DOUBLE, &
        c_loc(TargetValue),c_sizeof(TargetValue),StrLen_or_Ind)
  end function SQLGetDataDouble

  function SQLBindParameterDouble &
    (hstmt,ipar,fParamType,fSqlType, &
    cbColDef,ibScale,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    integer(kind=c_short),intent(in),value :: fSqlType
    integer(kind=c_long),intent(in),value :: cbColDef
    integer(kind=c_short),intent(in),value :: ibScale
    real(kind=c_double),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,SQL_DOUBLE,fSqlType, &
    cbColDef,ibScale,c_loc(rgbValue),c_sizeof(rgbValue),pcbValue)
  end function SQLBindParameterDouble

  function SQLBindParameterDouble_ &
    (hstmt,ipar,fParamType,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    real(kind=c_double),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,SQL_DOUBLE, &
    SQL_DOUBLE, 0, 0_2,c_loc(rgbValue),c_sizeof(rgbValue),pcbValue)
  end function SQLBindParameterDouble_

  function SQLBindParameterDouble__ &
    (hstmt,ipar,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    real(kind=c_double),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,SQL_PARAM_INPUT,SQL_DOUBLE, &
    SQL_DOUBLE, 0, 0_2,c_loc(rgbValue),c_sizeof(rgbValue),pcbValue)
  end function SQLBindParameterDouble__

  function SQLBindColTimeStamp &
       (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    type(SQL_TIMESTAMP_STRUCT),target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLBindCol0(StatementHandle,ColumnNumber,SQL_TYPE_TIMESTAMP, &
        c_loc(TargetValue),c_sizeof(TargetValue), StrLen_or_Ind)
  end function SQLBindColTimeStamp

  function SQLGetDataTimeStamp &
        (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    type(SQL_TIMESTAMP_STRUCT),target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLGetData0(StatementHandle,ColumnNumber,SQL_TYPE_TIMESTAMP, &
        c_loc(TargetValue),c_sizeof(TargetValue),StrLen_or_Ind)
  end function SQLGetDataTimeStamp

  function SQLBindParameterTimeStamp &
    (hstmt,ipar,fParamType,fSqlType, &
    cbColDef,ibScale,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    integer(kind=c_short),intent(in),value :: fSqlType
    integer(kind=c_long),intent(in),value :: cbColDef
    integer(kind=c_short),intent(in),value :: ibScale
    type(SQL_TIMESTAMP_STRUCT),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,SQL_TYPE_TIMESTAMP,fSqlType, &
    cbColDef,ibScale,c_loc(rgbValue),c_sizeof(rgbValue),pcbValue)
  end function SQLBindParameterTimeStamp

  function SQLBindParameterTimeStamp_ &
    (hstmt,ipar,fParamType,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    type(SQL_TIMESTAMP_STRUCT),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,SQL_TYPE_TIMESTAMP, &
    SQL_TYPE_TIMESTAMP, 0, 0_2,c_loc(rgbValue),c_sizeof(rgbValue),pcbValue)
  end function SQLBindParameterTimeStamp_

  function SQLBindParameterTimeStamp__ &
    (hstmt,ipar,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    type(SQL_TIMESTAMP_STRUCT),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,SQL_PARAM_INPUT,SQL_TYPE_TIMESTAMP, &
    SQL_TYPE_TIMESTAMP, 0, 0_2,c_loc(rgbValue),c_sizeof(rgbValue),pcbValue)
  end function SQLBindParameterTimeStamp__

  function SQLBindColDate &
       (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    type(SQL_DATE_STRUCT),target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLBindCol0(StatementHandle,ColumnNumber,SQL_TYPE_DATE, &
        c_loc(TargetValue),c_sizeof(TargetValue), StrLen_or_Ind)
  end function SQLBindColDate

  function SQLGetDataDate &
        (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    type(SQL_DATE_STRUCT),target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLGetData0(StatementHandle,ColumnNumber,SQL_TYPE_DATE, &
        c_loc(TargetValue),c_sizeof(TargetValue),StrLen_or_Ind)
  end function SQLGetDataDate

  function SQLBindParameterDate &
    (hstmt,ipar,fParamType,fSqlType, &
    cbColDef,ibScale,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    integer(kind=c_short),intent(in),value :: fSqlType
    integer(kind=c_long),intent(in),value :: cbColDef
    integer(kind=c_short),intent(in),value :: ibScale
    type(SQL_DATE_STRUCT),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,SQL_TYPE_DATE,fSqlType, &
    cbColDef,ibScale,c_loc(rgbValue),c_sizeof(rgbValue),pcbValue)
  end function SQLBindParameterDate

  function SQLBindParameterDate_ &
    (hstmt,ipar,fParamType,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    type(SQL_DATE_STRUCT),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,SQL_TYPE_DATE, &
    SQL_TYPE_DATE, 0, 0_2,c_loc(rgbValue),c_sizeof(rgbValue),pcbValue)
  end function SQLBindParameterDate_

  function SQLBindParameterDate__ &
    (hstmt,ipar,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    type(SQL_DATE_STRUCT),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,SQL_PARAM_INPUT,SQL_TYPE_DATE, &
    SQL_TYPE_DATE, 0, 0_2,c_loc(rgbValue),c_sizeof(rgbValue),pcbValue)
  end function SQLBindParameterDate__

end module fodbc
