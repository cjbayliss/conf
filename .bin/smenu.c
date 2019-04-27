/* 
 * Copyright (C) 2018-2019 Christopher Bayliss
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 */

/* gcc ~/.bin/smenu.c -o ~/.bin/smenu `pkg-config --cflags --libs vte-2.91` */

#include <stdio.h>
#include <gdk/gdk.h>
#include <vte/vte.h>

/* NOTE: we don't set any options, that way we get a better startup time */

int main(int argc, char *argv[]) {
	/* GtkWidget is a struct */
	GtkWidget *smenu_win, *smenu_vte;
	/* initialize and create the vte and window */
	gtk_init(&argc, &argv);
	smenu_vte = vte_terminal_new();
	smenu_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(smenu_win), "smenu");

	/* see https://github.com/swaywm/sway/issues/1367 */
	/* this SHOULD work with any shell, assuming you have either coreutils from the FSF or something simallar. you'll also need fzf */
	gchar **shell = (gchar *[]){"/bin/sh", "-c", "(IFS=':'; find $PATH -maxdepth 1;) | sort -u | fzf | xargs -r swaymsg -t command exec", NULL};

	/* wrt vte_terminal_spawn_async(), see:
	 * https://developer.gnome.org/vte/0.48/VteTerminal.html#vte-terminal-spawn-async */
	vte_terminal_spawn_async(VTE_TERMINAL(smenu_vte), VTE_PTY_DEFAULT, NULL, shell, NULL,
				 0,
				 NULL, NULL, NULL,
				 -1,
				 NULL, NULL, NULL);

	/* quit if closing the window or exiting the shell */
	g_signal_connect(smenu_win, "delete-event", gtk_main_quit, NULL);
	g_signal_connect(smenu_vte, "child-exited", gtk_main_quit, NULL);

	/* add the vte to the window, show the window */
	gtk_container_add(GTK_CONTAINER(smenu_win), smenu_vte);
	gtk_widget_show_all(smenu_win);
	gtk_main();
}
