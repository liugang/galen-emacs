#ifndef _ZOOM_RATIO_DEFINITION_H_

typedef enum _ZOOM_RATIO_IDX {
	ZOOM_NORMAL = 0,
	ZOOM_IN_1_1X, 	//1.1X
	ZOOM_IN_1_2X,	//1.2X
	ZOOM_IN_2X,
	ZOOM_IN_4X,
	ZOOM_IN_8X,
	ZOOM_OUT_2X,
	ZOOM_OUT_4X,
	ZOOM_OUT_8X,
	ZOOM_INVALID,
} ZOOM_RATIO_IDX;

typedef struct _ZOOM_RATIO_DATA {
	int idx;
	const char *default_name;
	double ratio;
} ZOOM_RATIO_DATA;

static ZOOM_RATIO_DATA ZoomRatioData[] =  //note: please make sure the order is the same with ZOOM_RATIO_IDX
{
	{ZOOM_NORMAL,"1X",1.0f},
	{ZOOM_IN_1_1X,"1.1X",1.1f},
	{ZOOM_IN_1_2X,"1.2X",1.2f},
	{ZOOM_IN_2X,"2X",2.0f},
	{ZOOM_IN_4X,"4X",4.0f},
	{ZOOM_IN_8X,"8X",8.0f},
	{ZOOM_OUT_2X,"1/2X",1.0/2},
	{ZOOM_OUT_4X,"1/4X",1.0/4},
	{ZOOM_OUT_8X,"1/8X",1.0/8},
	{ZOOM_INVALID,"invalid",1.0f},
};

typedef struct _ZOOM_RATIO_CUSTOM_DATA {
	int idx;
	int zoom_in_idx;
	int zoom_out_idx;
	const char *custom_name;
} ZOOM_RATIO_CUSTOM_DATA;

#define ZOOM_BEGIN_VALUE ZOOM_NORMAL 

#ifdef CONF_ZOOM_IN_OUT_CASE2
#define ZOOM_MAX_VAUE 9

static ZOOM_RATIO_CUSTOM_DATA ZoomRatioCustomZoomRule[] =
{
	{ZOOM_NORMAL,1,6,"1X"}, 	// 0 	: 1X -> 1.1X or 1/2X
	{ZOOM_IN_1_1X,2,0,"1.1X"}, 	// 1 	: 1.1X -> 1.2X or 1X
	{ZOOM_IN_1_2X,3,1,"1.2X"}, 	// 2 	: 1.2X -> 2X or 1.1X
	{ZOOM_IN_2X,4,2,"2X"},		// 3	: 2X -> 4X or 1.2X
	{ZOOM_IN_4X,5,3,"4X"},		// 4	: 4X -> 8X or 2X
	{ZOOM_IN_8X,0,4,"8X"},		// 5	: 8X -> 1X or 4X
	{ZOOM_OUT_2X,0,7,"1/2X"}, 	// 6	: 1/2 -> 1X or 1/4X
	{ZOOM_OUT_4X,6,8,"1/4X"},	// 7	: 1/4 -> 1/2 or 1/8X
	{ZOOM_OUT_8X,7,0,"1/8X"},	// 8	: 1/8 X -> 1/4X or 1X
	{ZOOM_INVALID,0,0,"1X"},	// 9
};
#else
#define ZOOM_MAX_VAUE 7

static ZOOM_RATIO_CUSTOM_DATA ZoomRatioCustomZoomRule[] =
{
	{ZOOM_NORMAL,1,4,"1X"}, 	// 0 	: 1X -> 2X or 1/2X
	{ZOOM_IN_2X,2,0,"2X"},		// 1	: 2X -> 4X or 1X
	{ZOOM_IN_4X,3,1,"4X"},		// 2	: 4X -> 8X or 2X
	{ZOOM_IN_8X,0,2,"8X"},		// 3	: 8X -> 1X or 4X
	{ZOOM_OUT_2X,0,5,"1/2X"}, 	// 4	: 1/2 -> 1X or 1/4X
	{ZOOM_OUT_4X,4,6,"1/4X"},	// 5	: 1/4 -> 1/2 or 1/8X
	{ZOOM_OUT_8X,5,0,"1/8X"},	// 6	: 1/8 X -> 1/4X or 1X
	{ZOOM_INVALID,0,0,"1X"},	// 7
};
#endif


#define GetZoomInValue(x) ((x<ZOOM_MAX_VAUE) ? ZoomRatioCustomZoomRule[x].zoom_in_idx : ZOOM_BEGIN_VALUE)
#define GetZoomOutValue(x) ((x<ZOOM_MAX_VAUE) ? ZoomRatioCustomZoomRule[x].zoom_out_idx: ZOOM_BEGIN_VALUE)
#define GetZoomText(x) ((x<ZOOM_MAX_VAUE) ? ZoomRatioCustomZoomRule[x].custom_name : "")
#define GetZoomRatioValue(x) ((x<ZOOM_MAX_VAUE) ? ZoomRatioData[ZoomRatioCustomZoomRule[x].idx].ratio : 1.0)


#endif
