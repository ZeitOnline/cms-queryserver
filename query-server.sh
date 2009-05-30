#!/bin/sh

export LANG=en_US.utf-8

DESC="ZEIT_online CMS-Query-Server"
NAME=cms-query
LISP=/usr/bin/sbcl
BASE=/usr/local/share/cms-query
IMAGE=$BASE/weblisp.core
SETUP=run-cms-query.lisp


export LC_ALL=en_US.UTF-8

test -f $LISP || exit 0

case "$1" in
  start)
        echo -n "Starting $DESC: ... "

	/usr/bin/detachtty --log-file $BASE/cms-query.log --pid-file $BASE/$NAME.pid $BASE/$NAME.socket \
     		$LISP --load $BASE/$SETUP

		
    echo Done 
        ;;
  stop)
        echo -n "Stopping $DESC: ... "
        if [ -e $BASE/$NAME.pid ]
        then
	    kill -TERM `cat $BASE/$NAME.pid`
            rm -f $BASE/$NAME.pid
            echo " Done"
   	else 
            echo "No $NAME found!"
	fi
	;;
  monitor)
	clear
	cat <<EOF

 To detach from this REPL send this process SIG-3
 (usually by typing Ctl-\)

 
EOF
        rlwrap attachtty $BASE/$NAME.socket
        ;;
  restart)
	$0 stop
	$0 start
  	;;
  moep)
 	$0 restart
	$0 monitor
	;;
  *)
        N=/etc/init.d/$NAME
        echo "Usage: $N {start|stop|restart|monitor}" >&2
        exit 1
        ;;
esac

exit 0



