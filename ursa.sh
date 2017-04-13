#!/bin/sh
#
# PROVIDE: ursa
# REQUIRE: LOGIN
# KEYWORD: shutdown
# 

. /etc/rc.subr

name=ursa
rcvar=ursa_enable

load_rc_config $name

: ${ursa_user:=ursa}
: ${ursa_enable:=NO}

start_cmd="ursa_start"
stop_cmd="ursa_stop"

ursa_start()
{
  rm -rf /tmp/erl_pipes
  mkdir /tmp/erl_pipes
  chown $ursa_user:$ursa_user /tmp/erl_pipes
  /usr/local/bin/sudo -u $ursa_user /home/ursa/ursa/bin/ursa start
}

ursa_stop()
{
  /usr/local/bin/sudo -u $ursa_user /home/ursa/ursa/bin/ursa stop
}

run_rc_command "$1"