/* gcc `pkg-config --cflags gtk+-3.0` -o window-default window-default.c `pkg-config --libs gtk+-3.0` */

/* To begin our introduction to GTK, we'll start with the simplest program
 * possible. This program will create an empty 200x200 pixel window:
 */

#include <gtk/gtk.h>

int
main (int   argc,
      char *argv[])
{
	/* we declare a window variable as a pointer of type GtkWidget. */
	GtkWidget *window;

	/* gtk_init() is the initialization function for GTK+; this function
	 * will set up GTK+, the type system, the connection to the windowing
	 * environment, etc.
	 */
	gtk_init (&argc, &argv);

	/* gtk_window_new() create a new GtkWindow and store it inside the
	 * window variable. The type of the window is GTK_WINDOW_TOPLEVEL, which
	 * means that the GtkWindow will be managed by the windowing system: it
	 * will have a frame, a title bar and window controls, depending on the
	 * platform.
	 */
	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);

	/* we connect the "destroy" signal to the gtk_main_quit() function. This
	 * function will terminate the GTK+ main loop started by calling
	 * gtk_main() later. The "destroy" signal is emitted when a widget is
	 * destroyed, by explicitly calling gtk_widget_destroy() or Close window
	 * control button is clicked.
	 */
	g_signal_connect (window, "destroy", G_CALLBACK (gtk_main_quit), NULL);

	/* GtkWidgets are hidden by default. By calling gtk_widget_show() on a
	 * GtkWidget we are asking GTK+ to set the visibility attribute so that
	 * it can be displayed. All this work is done after the main loop has
	 * been started.
	 */
	gtk_widget_show (window);

	/* This function will start the GTK+ main loop and will block the
	 * control flow of the main() until the gtk_main_quit() function is
	 * called.
	 */
	gtk_main ();

	return 0;
}

/* While the program is running, GTK+ is receiving events. These are typically
 * input events caused by the user interacting with your program, but also
 * things like messages from the window manager or other applications.	*/
