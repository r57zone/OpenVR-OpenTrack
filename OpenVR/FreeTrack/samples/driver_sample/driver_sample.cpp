//============ Copyright (c) Valve Corporation, All rights reserved. ============
//=============== Changed by r57zone (https://github.com/r57zone) ===============

#include <openvr_driver.h>
//#include "driverlog.h"

#include <vector>
#include <thread>
#include <chrono>
#include <algorithm>

#if defined( _WINDOWS )
#include <windows.h>
#endif

using namespace vr;


#if defined(_WIN32)
#define HMD_DLL_EXPORT extern "C" __declspec( dllexport )
#define HMD_DLL_IMPORT extern "C" __declspec( dllimport )
#elif defined(__GNUC__) || defined(COMPILER_GCC) || defined(__APPLE__)
#define HMD_DLL_EXPORT extern "C" __attribute__((visibility("default")))
#define HMD_DLL_IMPORT extern "C" 
#else
#error "Unsupported Platform."
#endif

inline HmdQuaternion_t HmdQuaternion_Init( double w, double x, double y, double z )
{
	HmdQuaternion_t quat;
	quat.w = w;
	quat.x = x;
	quat.y = y;
	quat.z = z;
	return quat;
}

inline void HmdMatrix_SetIdentity( HmdMatrix34_t *pMatrix )
{
	pMatrix->m[0][0] = 1.f;
	pMatrix->m[0][1] = 0.f;
	pMatrix->m[0][2] = 0.f;
	pMatrix->m[0][3] = 0.f;
	pMatrix->m[1][0] = 0.f;
	pMatrix->m[1][1] = 1.f;
	pMatrix->m[1][2] = 0.f;
	pMatrix->m[1][3] = 0.f;
	pMatrix->m[2][0] = 0.f;
	pMatrix->m[2][1] = 0.f;
	pMatrix->m[2][2] = 1.f;
	pMatrix->m[2][3] = 0.f;
}


// keys for use with the settings API
static const char * const k_pch_OpenTrack_Section = "opentrack";
static const char * const k_pch_OpenTrack_SerialNumber_String = "serialNumber";
static const char * const k_pch_OpenTrack_ModelNumber_String = "modelNumber";
static const char * const k_pch_OpenTrack_WindowX_Int32 = "windowX";
static const char * const k_pch_OpenTrack_WindowY_Int32 = "windowY";
static const char * const k_pch_OpenTrack_WindowWidth_Int32 = "windowWidth";
static const char * const k_pch_OpenTrack_WindowHeight_Int32 = "windowHeight";
static const char * const k_pch_OpenTrack_RenderWidth_Int32 = "renderWidth";
static const char * const k_pch_OpenTrack_RenderHeight_Int32 = "renderHeight";
static const char * const k_pch_OpenTrack_SecondsFromVsyncToPhotons_Float = "secondsFromVsyncToPhotons";
static const char * const k_pch_OpenTrack_DisplayFrequency_Float = "displayFrequency";

static const char * const k_pch_OpenTrack_DistortionK1_Float = "DistortionK1";
static const char * const k_pch_OpenTrack_DistortionK2_Float = "DistortionK2";
static const char * const k_pch_OpenTrack_ZoomWidth_Float = "ZoomWidth";
static const char * const k_pch_OpenTrack_ZoomHeight_Float = "ZoomHeight";
static const char * const k_pch_OpenTrack_FOV_Float = "FOV";
static const char * const k_pch_OpenTrack_DistanceBetweenEyes_Int32 = "DistanceBetweenEyes";
static const char * const k_pch_OpenTrack_ScreenOffsetX_Int32 = "ScreenOffsetX";
static const char * const k_pch_OpenTrack_DebugMode_Bool = "DebugMode";
static const char * const k_pch_OpenTrack_CrouchPressKey_String = "CrouchPressKey";
static const char * const k_pch_OpenTrack_CrouchOffset_Float = "CrouchOffset";


#define FREETRACK_HEAP "FT_SharedMem"
#define FREETRACK_MUTEX "FT_Mutext"

/* only 6 headpose floats and the data id are filled -sh */
typedef struct FTData__ {
	uint32_t DataID;
	int32_t CamWidth;
	int32_t CamHeight;
	/* virtual pose */
	float  Yaw;   /* positive yaw to the left */
	float  Pitch; /* positive pitch up */
	float  Roll;  /* positive roll to the left */
	float  X;
	float  Y;
	float  Z;
	/* raw pose with no smoothing, sensitivity, response curve etc. */
	float  RawYaw;
	float  RawPitch;
	float  RawRoll;
	float  RawX;
	float  RawY;
	float  RawZ;
	/* raw points, sorted by Y, origin top left corner */
	float  X1;
	float  Y1;
	float  X2;
	float  Y2;
	float  X3;
	float  Y3;
	float  X4;
	float  Y4;
} volatile FTData;

typedef struct FTHeap__ {
	FTData data;
	int32_t GameID;
	union
	{
		unsigned char table[8];
		int32_t table_ints[2];
	};
	int32_t GameID2;
} volatile FTHeap;

static HANDLE hFTMemMap = 0;
static FTHeap *ipc_heap = 0;
static HANDLE ipc_mutex = 0;

FTData *FreeTrack;
bool HMDConnected = false;
std::thread *pFTthread = NULL;

inline vr::HmdQuaternion_t EulerAngleToQuaternion(double Yaw, double Pitch, double Roll)
{
	vr::HmdQuaternion_t q;
	// Abbreviations for the various angular functions
	double cy = cos(Yaw * 0.5);
	double sy = sin(Yaw * 0.5);
	double cp = cos(Pitch * 0.5);
	double sp = sin(Pitch * 0.5);
	double cr = cos(Roll * 0.5);
	double sr = sin(Roll * 0.5);

	q.w = cr * cp * cy + sr * sp * sy;
	q.x = sr * cp * cy - cr * sp * sy;
	q.y = cr * sp * cy + sr * cp * sy;
	q.z = cr * cp * sy - sr * sp * cy;

	return q;
}

//FreeTrack implementation from OpenTrack (https://github.com/opentrack/opentrack/tree/unstable/freetrackclient)
static BOOL impl_create_mapping(void)
{
	if (ipc_heap != NULL)
		return TRUE;

	hFTMemMap = CreateFileMappingA(INVALID_HANDLE_VALUE,
		NULL,
		PAGE_READWRITE,
		0,
		sizeof(FTHeap),
		(LPCSTR)FREETRACK_HEAP);

	if (hFTMemMap == NULL)
		return (ipc_heap = NULL), FALSE;

	ipc_heap = (FTHeap*)MapViewOfFile(hFTMemMap, FILE_MAP_WRITE, 0, 0, sizeof(FTHeap));
	ipc_mutex = CreateMutexA(NULL, FALSE, FREETRACK_MUTEX);

	return TRUE;
}

void FTRead()
{
	while (HMDConnected) {
		if (ipc_mutex && WaitForSingleObject(ipc_mutex, 16) == WAIT_OBJECT_0) {
			memcpy(&FreeTrack, &ipc_heap, sizeof(FreeTrack));
			if (ipc_heap->data.DataID > (1 << 29))
				ipc_heap->data.DataID = 0;
			ReleaseMutex(ipc_mutex);
		}
	}
}

int KeyNameToKeyCode(std::string KeyName) {
	std::transform(KeyName.begin(), KeyName.end(), KeyName.begin(), ::toupper);

	if (KeyName == "NONE") return 0;

	else if (KeyName == "MOUSE-LEFT-BTN") return VK_LBUTTON;
	else if (KeyName == "MOUSE-RIGHT-BTN") return VK_RBUTTON;
	else if (KeyName == "MOUSE-MIDDLE-BTN") return VK_MBUTTON;
	else if (KeyName == "MOUSE-SIDE1-BTN") return VK_XBUTTON1;
	else if (KeyName == "MOUSE-SIDE2-BTN") return VK_XBUTTON2;

	else if (KeyName == "ESCAPE") return VK_ESCAPE;
	else if (KeyName == "F1") return VK_F1;
	else if (KeyName == "F2") return VK_F2;
	else if (KeyName == "F3") return VK_F3;
	else if (KeyName == "F4") return VK_F4;
	else if (KeyName == "F5") return VK_F5;
	else if (KeyName == "F6") return VK_F6;
	else if (KeyName == "F7") return VK_F7;
	else if (KeyName == "F8") return VK_F8;
	else if (KeyName == "F9") return VK_F9;
	else if (KeyName == "F10") return VK_F10;
	else if (KeyName == "F11") return VK_F11;
	else if (KeyName == "F12") return VK_F12;

	else if (KeyName == "~") return 192;
	else if (KeyName == "1") return '1';
	else if (KeyName == "2") return '2';
	else if (KeyName == "3") return '3';
	else if (KeyName == "4") return '4';
	else if (KeyName == "5") return '5';
	else if (KeyName == "6") return '6';
	else if (KeyName == "7") return '7';
	else if (KeyName == "8") return '8';
	else if (KeyName == "9") return '9';
	else if (KeyName == "0") return '0';
	else if (KeyName == "-") return 189;
	else if (KeyName == "=") return 187;

	else if (KeyName == "TAB") return VK_TAB;
	else if (KeyName == "CAPS-LOCK") return VK_CAPITAL;
	else if (KeyName == "SHIFT") return VK_SHIFT;
	else if (KeyName == "CTRL") return VK_CONTROL;
	else if (KeyName == "WIN") return VK_LWIN;
	else if (KeyName == "ALT") return VK_MENU;
	else if (KeyName == "SPACE") return VK_SPACE;
	else if (KeyName == "ENTER") return VK_RETURN;
	else if (KeyName == "BACKSPACE") return VK_BACK;

	else if (KeyName == "Q") return 'Q';
	else if (KeyName == "W") return 'W';
	else if (KeyName == "E") return 'E';
	else if (KeyName == "R") return 'R';
	else if (KeyName == "T") return 'T';
	else if (KeyName == "Y") return 'Y';
	else if (KeyName == "U") return 'U';
	else if (KeyName == "I") return 'I';
	else if (KeyName == "O") return 'O';
	else if (KeyName == "P") return 'P';
	else if (KeyName == "[") return 219;
	else if (KeyName == "]") return 221;
	else if (KeyName == "A") return 'A';
	else if (KeyName == "S") return 'S';
	else if (KeyName == "D") return 'D';
	else if (KeyName == "F") return 'F';
	else if (KeyName == "G") return 'G';
	else if (KeyName == "H") return 'H';
	else if (KeyName == "J") return 'J';
	else if (KeyName == "K") return 'K';
	else if (KeyName == "L") return 'L';
	else if (KeyName == ":") return 186;
	else if (KeyName == "APOSTROPHE") return 222;
	else if (KeyName == "\\") return 220;
	else if (KeyName == "Z") return 'Z';
	else if (KeyName == "X") return 'X';
	else if (KeyName == "C") return 'C';
	else if (KeyName == "V") return 'V';
	else if (KeyName == "B") return 'B';
	else if (KeyName == "N") return 'N';
	else if (KeyName == "M") return 'M';
	else if (KeyName == "<") return 188;
	else if (KeyName == ">") return 190;
	else if (KeyName == "?") return 191;

	else if (KeyName == "PRINTSCREEN") return VK_SNAPSHOT;
	else if (KeyName == "SCROLL-LOCK") return VK_SCROLL;
	else if (KeyName == "PAUSE") return VK_PAUSE;
	else if (KeyName == "INSERT") return VK_INSERT;
	else if (KeyName == "HOME") return VK_HOME;
	else if (KeyName == "PAGE-UP") return VK_NEXT;
	else if (KeyName == "DELETE") return VK_DELETE;
	else if (KeyName == "END") return VK_END;
	else if (KeyName == "PAGE-DOWN") return VK_PRIOR;

	else if (KeyName == "UP") return VK_UP;
	else if (KeyName == "DOWN") return VK_DOWN;
	else if (KeyName == "LEFT") return VK_LEFT;
	else if (KeyName == "RIGHT") return VK_RIGHT;

	else if (KeyName == "NUM-LOCK") return VK_NUMLOCK;
	else if (KeyName == "NUMPAD0") return VK_NUMPAD0;
	else if (KeyName == "NUMPAD1") return VK_NUMPAD1;
	else if (KeyName == "NUMPAD2") return VK_NUMPAD2;
	else if (KeyName == "NUMPAD3") return VK_NUMPAD3;
	else if (KeyName == "NUMPAD4") return VK_NUMPAD4;
	else if (KeyName == "NUMPAD5") return VK_NUMPAD5;
	else if (KeyName == "NUMPAD6") return VK_NUMPAD6;
	else if (KeyName == "NUMPAD7") return VK_NUMPAD7;
	else if (KeyName == "NUMPAD8") return VK_NUMPAD8;
	else if (KeyName == "NUMPAD9") return VK_NUMPAD9;

	else if (KeyName == "NUMPAD-DIVIDE") return VK_DIVIDE;
	else if (KeyName == "NUMPAD-MULTIPLY") return VK_MULTIPLY;
	else if (KeyName == "NUMPAD-MINUS") return VK_SUBTRACT;
	else if (KeyName == "NUMPAD-PLUS") return VK_ADD;
	else if (KeyName == "NUMPAD-DEL") return VK_DECIMAL;

	else return 0;
}

//-----------------------------------------------------------------------------
// Purpose:
//-----------------------------------------------------------------------------
class CSampleDeviceDriver : public vr::ITrackedDeviceServerDriver, public vr::IVRDisplayComponent
{
public:
	CSampleDeviceDriver(  )
	{
		m_unObjectId = vr::k_unTrackedDeviceIndexInvalid;
		m_ulPropertyContainer = vr::k_ulInvalidPropertyContainer;

		//DriverLog( "Using settings values\n" );
		m_flIPD = vr::VRSettings()->GetFloat( k_pch_SteamVR_Section, k_pch_SteamVR_IPD_Float );

		char buf[1024];
		vr::VRSettings()->GetString( k_pch_OpenTrack_Section, k_pch_OpenTrack_SerialNumber_String, buf, sizeof( buf ) );
		m_sSerialNumber = buf;

		vr::VRSettings()->GetString( k_pch_OpenTrack_Section, k_pch_OpenTrack_ModelNumber_String, buf, sizeof( buf ) );
		m_sModelNumber = buf;

		m_nWindowX = vr::VRSettings()->GetInt32( k_pch_OpenTrack_Section, k_pch_OpenTrack_WindowX_Int32 );
		m_nWindowY = vr::VRSettings()->GetInt32( k_pch_OpenTrack_Section, k_pch_OpenTrack_WindowY_Int32 );
		m_nWindowWidth = vr::VRSettings()->GetInt32( k_pch_OpenTrack_Section, k_pch_OpenTrack_WindowWidth_Int32 );
		m_nWindowHeight = vr::VRSettings()->GetInt32( k_pch_OpenTrack_Section, k_pch_OpenTrack_WindowHeight_Int32 );
		m_nRenderWidth = vr::VRSettings()->GetInt32( k_pch_OpenTrack_Section, k_pch_OpenTrack_RenderWidth_Int32 );
		m_nRenderHeight = vr::VRSettings()->GetInt32( k_pch_OpenTrack_Section, k_pch_OpenTrack_RenderHeight_Int32 );
		m_flSecondsFromVsyncToPhotons = vr::VRSettings()->GetFloat( k_pch_OpenTrack_Section, k_pch_OpenTrack_SecondsFromVsyncToPhotons_Float );
		m_flDisplayFrequency = vr::VRSettings()->GetFloat( k_pch_OpenTrack_Section, k_pch_OpenTrack_DisplayFrequency_Float );

		m_fDistortionK1 = vr::VRSettings()->GetFloat(k_pch_OpenTrack_Section, k_pch_OpenTrack_DistortionK1_Float);
		m_fDistortionK2 = vr::VRSettings()->GetFloat(k_pch_OpenTrack_Section, k_pch_OpenTrack_DistortionK2_Float);
		m_fZoomWidth = vr::VRSettings()->GetFloat(k_pch_OpenTrack_Section, k_pch_OpenTrack_ZoomWidth_Float);
		m_fZoomHeight = vr::VRSettings()->GetFloat(k_pch_OpenTrack_Section, k_pch_OpenTrack_ZoomHeight_Float);
		m_fFOV = (vr::VRSettings()->GetFloat(k_pch_OpenTrack_Section, k_pch_OpenTrack_FOV_Float) * 3.14159265358979323846 / 180); //radians
		m_nDistanceBetweenEyes = vr::VRSettings()->GetFloat(k_pch_OpenTrack_Section, k_pch_OpenTrack_DistanceBetweenEyes_Int32);
		m_nScreenOffsetX = vr::VRSettings()->GetFloat(k_pch_OpenTrack_Section, k_pch_OpenTrack_ScreenOffsetX_Int32);
		m_bDebugMode = vr::VRSettings()->GetBool(k_pch_OpenTrack_Section, k_pch_OpenTrack_DebugMode_Bool);

		vr::VRSettings()->GetString(k_pch_OpenTrack_Section, k_pch_OpenTrack_CrouchPressKey_String, buf, sizeof(buf));
		m_crouchPressKey = KeyNameToKeyCode(buf);

		m_crouchOffset = vr::VRSettings()->GetFloat(k_pch_OpenTrack_Section, k_pch_OpenTrack_CrouchOffset_Float);

		/*DriverLog( "driver_null: Serial Number: %s\n", m_sSerialNumber.c_str() );
		DriverLog( "driver_null: Model Number: %s\n", m_sModelNumber.c_str() );
		DriverLog( "driver_null: Window: %d %d %d %d\n", m_nWindowX, m_nWindowY, m_nWindowWidth, m_nWindowHeight );
		DriverLog( "driver_null: Render Target: %d %d\n", m_nRenderWidth, m_nRenderHeight );
		DriverLog( "driver_null: Seconds from Vsync to Photons: %f\n", m_flSecondsFromVsyncToPhotons );
		DriverLog( "driver_null: Display Frequency: %f\n", m_flDisplayFrequency );
		DriverLog( "driver_null: IPD: %f\n", m_flIPD );*/
	}

	virtual ~CSampleDeviceDriver()
	{
	}


	virtual EVRInitError Activate( vr::TrackedDeviceIndex_t unObjectId ) 
	{
		m_unObjectId = unObjectId;
		m_ulPropertyContainer = vr::VRProperties()->TrackedDeviceToPropertyContainer( m_unObjectId );


		vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, Prop_ModelNumber_String, m_sModelNumber.c_str() );
		vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, Prop_RenderModelName_String, m_sModelNumber.c_str() );
		
		vr::VRProperties()->SetStringProperty(m_ulPropertyContainer, Prop_SerialNumber_String, m_sSerialNumber.c_str());
		vr::VRProperties()->SetBoolProperty(m_ulPropertyContainer, Prop_WillDriftInYaw_Bool, false);
		vr::VRProperties()->SetStringProperty(m_ulPropertyContainer, Prop_ManufacturerName_String, "OpenVR-OpenTrack");
		vr::VRProperties()->SetStringProperty(m_ulPropertyContainer, Prop_TrackingFirmwareVersion_String, "1.0");
		vr::VRProperties()->SetStringProperty(m_ulPropertyContainer, Prop_HardwareRevision_String, "1.0");
		vr::VRProperties()->SetBoolProperty(m_ulPropertyContainer, Prop_DeviceIsWireless_Bool, false);
		vr::VRProperties()->SetBoolProperty(m_ulPropertyContainer, Prop_DeviceIsCharging_Bool, false);
		vr::VRProperties()->SetBoolProperty(m_ulPropertyContainer, Prop_CanUnifyCoordinateSystemWithHmd_Bool, true);
		vr::VRProperties()->SetBoolProperty(m_ulPropertyContainer, Prop_ContainsProximitySensor_Bool, false);
		vr::VRProperties()->SetBoolProperty(m_ulPropertyContainer, Prop_DeviceCanPowerOff_Bool, false);
		vr::VRProperties()->SetInt32Property(m_ulPropertyContainer, vr::Prop_DeviceClass_Int32, vr::TrackedDeviceClass_HMD);
		vr::VRProperties()->SetBoolProperty(m_ulPropertyContainer, vr::Prop_HasCamera_Bool, false);
		vr::VRProperties()->SetBoolProperty(m_ulPropertyContainer, vr::Prop_Firmware_ForceUpdateRequired_Bool, false);
		vr::VRProperties()->SetStringProperty(m_ulPropertyContainer, vr::Prop_RegisteredDeviceType_String, "HMD");
		vr::VRProperties()->SetBoolProperty(m_ulPropertyContainer, vr::Prop_NeverTracked_Bool, false);
		vr::VRProperties()->SetBoolProperty(m_ulPropertyContainer, vr::Prop_Identifiable_Bool, false);

		vr::VRProperties()->SetFloatProperty( m_ulPropertyContainer, Prop_UserIpdMeters_Float, m_flIPD );
		vr::VRProperties()->SetFloatProperty( m_ulPropertyContainer, Prop_UserHeadToEyeDepthMeters_Float, 0.f );
		vr::VRProperties()->SetFloatProperty( m_ulPropertyContainer, Prop_DisplayFrequency_Float, m_flDisplayFrequency );
		vr::VRProperties()->SetFloatProperty( m_ulPropertyContainer, Prop_SecondsFromVsyncToPhotons_Float, m_flSecondsFromVsyncToPhotons );

		// return a constant that's not 0 (invalid) or 1 (reserved for Oculus)
		vr::VRProperties()->SetUint64Property( m_ulPropertyContainer, Prop_CurrentUniverseId_Uint64, 2 );

		// avoid "not fullscreen" warnings from vrmonitor
		vr::VRProperties()->SetBoolProperty( m_ulPropertyContainer, Prop_IsOnDesktop_Bool, false );

		//Debug mode activate Windowed Mode (borderless fullscreen), lock to 30 FPS 
		vr::VRProperties()->SetBoolProperty(m_ulPropertyContainer, Prop_DisplayDebugMode_Bool, m_bDebugMode);

		// Icons can be configured in code or automatically configured by an external file "drivername\resources\driver.vrresources".
		// Icon properties NOT configured in code (post Activate) are then auto-configured by the optional presence of a driver's "drivername\resources\driver.vrresources".
		// In this manner a driver can configure their icons in a flexible data driven fashion by using an external file.
		//
		// The structure of the driver.vrresources file allows a driver to specialize their icons based on their HW.
		// Keys matching the value in "Prop_ModelNumber_String" are considered first, since the driver may have model specific icons.
		// An absence of a matching "Prop_ModelNumber_String" then considers the ETrackedDeviceClass ("HMD", "Controller", "GenericTracker", "TrackingReference")
		// since the driver may have specialized icons based on those device class names.
		//
		// An absence of either then falls back to the "system.vrresources" where generic device class icons are then supplied.
		//
		// Please refer to "bin\drivers\sample\resources\driver.vrresources" which contains this sample configuration.
		//
		// "Alias" is a reserved key and specifies chaining to another json block.
		//
		// In this sample configuration file (overly complex FOR EXAMPLE PURPOSES ONLY)....
		//
		// "Model-v2.0" chains through the alias to "Model-v1.0" which chains through the alias to "Model-v Defaults".
		//
		// Keys NOT found in "Model-v2.0" would then chase through the "Alias" to be resolved in "Model-v1.0" and either resolve their or continue through the alias.
		// Thus "Prop_NamedIconPathDeviceAlertLow_String" in each model's block represent a specialization specific for that "model".
		// Keys in "Model-v Defaults" are an example of mapping to the same states, and here all map to "Prop_NamedIconPathDeviceOff_String".
		//
		/*bool bSetupIconUsingExternalResourceFile = true;
		if ( !bSetupIconUsingExternalResourceFile )
		{
			// Setup properties directly in code.
			// Path values are of the form {drivername}\icons\some_icon_filename.png
			vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, vr::Prop_NamedIconPathDeviceOff_String, "{sample}/icons/headset_sample_status_off.png" );
			vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, vr::Prop_NamedIconPathDeviceSearching_String, "{sample}/icons/headset_sample_status_searching.gif" );
			vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, vr::Prop_NamedIconPathDeviceSearchingAlert_String, "{sample}/icons/headset_sample_status_searching_alert.gif" );
			vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, vr::Prop_NamedIconPathDeviceReady_String, "{sample}/icons/headset_sample_status_ready.png" );
			vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, vr::Prop_NamedIconPathDeviceReadyAlert_String, "{sample}/icons/headset_sample_status_ready_alert.png" );
			vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, vr::Prop_NamedIconPathDeviceNotReady_String, "{sample}/icons/headset_sample_status_error.png" );
			vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, vr::Prop_NamedIconPathDeviceStandby_String, "{sample}/icons/headset_sample_status_standby.png" );
			vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, vr::Prop_NamedIconPathDeviceAlertLow_String, "{sample}/icons/headset_sample_status_ready_low.png" );
		}*/

		return VRInitError_None;
	}

	virtual void Deactivate() 
	{
		m_unObjectId = vr::k_unTrackedDeviceIndexInvalid;
	}

	virtual void EnterStandby()
	{
	}

	void *GetComponent( const char *pchComponentNameAndVersion )
	{
		if ( !_stricmp( pchComponentNameAndVersion, vr::IVRDisplayComponent_Version ) )
		{
			return (vr::IVRDisplayComponent*)this;
		}

		// override this to add a component to a driver
		return NULL;
	}

	virtual void PowerOff() 
	{
	}

	/** debug request from a client */
	virtual void DebugRequest( const char *pchRequest, char *pchResponseBuffer, uint32_t unResponseBufferSize ) 
	{
		if( unResponseBufferSize >= 1 )
			pchResponseBuffer[0] = 0;
	}

	virtual void GetWindowBounds( int32_t *pnX, int32_t *pnY, uint32_t *pnWidth, uint32_t *pnHeight ) 
	{
		*pnX = m_nWindowX;
		*pnY = m_nWindowY;
		*pnWidth = m_nWindowWidth;
		*pnHeight = m_nWindowHeight;
	}

	virtual bool IsDisplayOnDesktop() 
	{
		return true;
	}

	virtual bool IsDisplayRealDisplay() 
	{
		if (m_nWindowX == 0 && m_nWindowY == 0)
			return false;
		else
			return true; //Support working on extended display
	}

	virtual void GetRecommendedRenderTargetSize( uint32_t *pnWidth, uint32_t *pnHeight ) 
	{
		*pnWidth = m_nRenderWidth;
		*pnHeight = m_nRenderHeight;
	}

	virtual void GetEyeOutputViewport( EVREye eEye, uint32_t *pnX, uint32_t *pnY, uint32_t *pnWidth, uint32_t *pnHeight ) 
	{
		*pnY = m_nScreenOffsetX;
		*pnWidth = m_nWindowWidth / 2;
		*pnHeight = m_nWindowHeight;

		if (eEye == Eye_Left)
		{
			*pnX = m_nDistanceBetweenEyes;
		}
		else
		{
			*pnX = (m_nWindowWidth / 2) - m_nDistanceBetweenEyes;
		}
	}

	virtual void GetProjectionRaw( EVREye eEye, float *pfLeft, float *pfRight, float *pfTop, float *pfBottom ) 
	{
		*pfLeft = -m_fFOV;
		*pfRight = m_fFOV;
		*pfTop = -m_fFOV;
		*pfBottom = m_fFOV;
	}

	virtual DistortionCoordinates_t ComputeDistortion( EVREye eEye, float fU, float fV ) 
	{
		DistortionCoordinates_t coordinates;

		//Distortion for lens implementation from https://github.com/HelenXR/openvr_survivor/blob/master/src/head_mount_display_device.cc
		float hX;
		float hY;
		double rr;
		double r2;
		double theta;

		rr = sqrt((fU - 0.5f)*(fU - 0.5f) + (fV - 0.5f)*(fV - 0.5f));
		r2 = rr * (1 + m_fDistortionK1 * (rr*rr) + m_fDistortionK2 * (rr*rr*rr*rr));
		theta = atan2(fU - 0.5f, fV - 0.5f);
		hX = sin(theta)*r2*m_fZoomWidth;
		hY = cos(theta)*r2*m_fZoomHeight;

		coordinates.rfBlue[0] = hX + 0.5f;
		coordinates.rfBlue[1] = hY + 0.5f;
		coordinates.rfGreen[0] = hX + 0.5f;
		coordinates.rfGreen[1] = hY + 0.5f;
		coordinates.rfRed[0] = hX + 0.5f;
		coordinates.rfRed[1] = hY + 0.5f;

		return coordinates;
	}

	virtual DriverPose_t GetPose() 
	{
		DriverPose_t pose = { 0 };
		if (HMDConnected) {
			pose.poseIsValid = true;
			pose.result = TrackingResult_Running_OK;
			pose.deviceIsConnected = true;
		}
		else {
			pose.poseIsValid = false;
			pose.result = TrackingResult_Uninitialized;
			pose.deviceIsConnected = false;
		}

		pose.qWorldFromDriverRotation = HmdQuaternion_Init(1, 0, 0, 0);
		pose.qDriverFromHeadRotation = HmdQuaternion_Init(1, 0, 0, 0);
		pose.shouldApplyHeadModel = true;
		pose.poseTimeOffset = 0;
		pose.willDriftInYaw = false;

		if (HMDConnected) {
			//Set head tracking rotation
			pose.qRotation = EulerAngleToQuaternion(FreeTrack->Roll, -FreeTrack->Yaw, FreeTrack->Pitch);

			//Set position tracking
			pose.vecPosition[0] = FreeTrack->X * 0.001; //millimeters to meters
			pose.vecPosition[1] = FreeTrack->Z * 0.001; //millimeters to meters 
			if ((GetAsyncKeyState(m_crouchPressKey) & 0x8000) != 0)
				pose.vecPosition[1] -= m_crouchOffset;
			pose.vecPosition[2] = FreeTrack->Y * 0.001; //millimeters to meters 
		}

		return pose;
	}
	

	void RunFrame()
	{
		// In a real driver, this should happen from some pose tracking thread.
		// The RunFrame interval is unspecified and can be very irregular if some other
		// driver blocks it for some periodic task.
		if ( m_unObjectId != vr::k_unTrackedDeviceIndexInvalid )
		{
			vr::VRServerDriverHost()->TrackedDevicePoseUpdated( m_unObjectId, GetPose(), sizeof( DriverPose_t ) );
		}
	}

	std::string GetSerialNumber() const { return m_sSerialNumber; }

private:
	vr::TrackedDeviceIndex_t m_unObjectId;
	vr::PropertyContainerHandle_t m_ulPropertyContainer;

	std::string m_sSerialNumber;
	std::string m_sModelNumber;

	int32_t m_nWindowX;
	int32_t m_nWindowY;
	int32_t m_nWindowWidth;
	int32_t m_nWindowHeight;
	int32_t m_nRenderWidth;
	int32_t m_nRenderHeight;
	float m_flSecondsFromVsyncToPhotons;
	float m_flDisplayFrequency;
	float m_flIPD;

	float m_fDistortionK1;
	float m_fDistortionK2;
	float m_fZoomWidth;
	float m_fZoomHeight;
	float m_fFOV;
	int32_t m_nDistanceBetweenEyes;
	int32_t m_nScreenOffsetX;
	bool m_bDebugMode;

	int32_t m_crouchPressKey;
	float m_crouchOffset;
};

//-----------------------------------------------------------------------------
// Purpose:
//-----------------------------------------------------------------------------
class CServerDriver_Sample: public IServerTrackedDeviceProvider
{
public:
	virtual EVRInitError Init( vr::IVRDriverContext *pDriverContext ) ;
	virtual void Cleanup() ;
	virtual const char * const *GetInterfaceVersions() { return vr::k_InterfaceVersions; }
	virtual void RunFrame() ;
	virtual bool ShouldBlockStandbyMode()  { return false; }
	virtual void EnterStandby()  {}
	virtual void LeaveStandby()  {}

private:
	CSampleDeviceDriver *m_pNullHmdLatest = nullptr;
};

CServerDriver_Sample g_serverDriverNull;


EVRInitError CServerDriver_Sample::Init( vr::IVRDriverContext *pDriverContext )
{
	VR_INIT_SERVER_DRIVER_CONTEXT( pDriverContext );
	//InitDriverLog( vr::VRDriverLog() );

	if (impl_create_mapping() == false) {
		HMDConnected = false;
	}
	else {
		HMDConnected = true;
		pFTthread = new std::thread(FTRead);
	}

	m_pNullHmdLatest = new CSampleDeviceDriver();
	vr::VRServerDriverHost()->TrackedDeviceAdded( m_pNullHmdLatest->GetSerialNumber().c_str(), vr::TrackedDeviceClass_HMD, m_pNullHmdLatest );

	return VRInitError_None;
}

void CServerDriver_Sample::Cleanup() 
{
	if (HMDConnected) {
		HMDConnected = false;
		if (pFTthread) {
			pFTthread->join();
			delete pFTthread;
			pFTthread = nullptr;
		}
	}
	//CleanupDriverLog();
	delete m_pNullHmdLatest;
	m_pNullHmdLatest = NULL;
}


void CServerDriver_Sample::RunFrame()
{
	if ( m_pNullHmdLatest )
	{
		m_pNullHmdLatest->RunFrame();
	}
}

//-----------------------------------------------------------------------------
// Purpose:
//-----------------------------------------------------------------------------
HMD_DLL_EXPORT void *HmdDriverFactory( const char *pInterfaceName, int *pReturnCode )
{
	if( 0 == strcmp( IServerTrackedDeviceProvider_Version, pInterfaceName ) )
	{
		return &g_serverDriverNull;
	}

	if( pReturnCode )
		*pReturnCode = VRInitError_Init_InterfaceNotFound;

	return NULL;
}
