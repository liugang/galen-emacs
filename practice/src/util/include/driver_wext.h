struct SITE_OBJ *wireless_driver_wext_get_scan_results(int *cnt, WIRELESS_MODE mode);
void wireless_driver_wext_scan_completed();
int wireless_driver_wext_set_ssid(const char *ssid, int ssid_len);
int wireless_driver_wext_scan(const char *ssid, int ssid_len);
int wireless_driver_wext_get_signal_strength(int *signal);
