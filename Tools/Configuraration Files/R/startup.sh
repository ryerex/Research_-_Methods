#!/bin/sh
##
# Depending on the system configuration, it can be required to set:
#     S_HOSTADDRESS
# 
# The authentication is set to 'fx' (method using SSH) by default.
##
# Usage of this script:
#     startup.sh <address> [options]
#     startup.sh <name> [options]
# It starts the R server asynchronously in background by default.
#     <address>                        the complete RMI-address
#     <name>                           the session name only (the hostname
#                                      should be set in this file)
# Options:
#     -wd=<working directory>          initial R working directory
#     -debug                           enables debug output and
#                                      runs R in foreground
#
# Note: Launch RMI manually: $JAVA_HOME/bin/rmiregistry &
# Author: Stephan Wahlbrink
###############################################################################

###############################################################################
# SCRIPT - INIT / READING PARAMETERS ##########################################
###############################################################################
if [ -z "$1" ]
then
	echo Missing name for R server
	exit -1
fi
ADDRESS=$1
S_NAME=`basename $ADDRESS`
S_HOSTADDRESS=127.0.0.1
S_REGISTRYPORT=
if [ "$ADDRESS" != "$S_NAME" ]
then
	S_HOSTADDRESS=`expr "$ADDRESS" : "[^/]*\/\/\([^:/]*\).*"`
	S_REGISTRYPORT=`expr "$ADDRESS" : "[^/]*\/\/[^:/]*:\([0-9]\+\)\/.*"`
fi
shift
WD=~
SCRIPT=`readlink -f "$0"`

until [ -z "$1" ]  # Until all parameters used up
do
	case "$1" in
	-wd=*)
		WD=${1##-wd=}
		;;
#	-host=*)
#		S_HOSTADDRESS=${1##-host=}
#		;;
	-debug*)
		DEBUG=1
		echo Debug mode enabled
		;;
	-dev*)
		DEV=1
		echo Development mode enabled
		;;
	*)
		echo Unknown parameter: $1
		;;
	esac
	shift
done
###############################################################################

###############################################################################
# CONFIG - SYSTEM SETTINGS ####################################################
###############################################################################
# Configure the startup in the following sections
###############################################################################
# R Related #
#############
R_DOC_DIR=/usr/lib64/R/R_DOC_DIR
R_SHARE_DIR=/usr/lib64/R/R_SHARE_DIR
R_INCLUDE_DIR=/usr/lib64/R/R_INCLUDE_DIR
R_LIBS_SITE=/usr/lib64/R/R_LIBS_SITE
R_LIBS=/usr/lib64/R/libs
R_HOME=/usr/lib64/R
################
# JAVA Related #
################
JAVA_HOME=/usr/lib/jvm/java-1.6.0-openjdk-1.6.0.36.x86_64/jre
####################
# R Server Related #
####################
#     RJS_HOME=`dirname "$SCRIPT"`
#     RJS_WORK=~/.RJServer
RJS_HOME=`dirname "$SCRIPT"`
RJS_WORK=~/.RJServer

###############################################################################
# Set explicitly the hostname and port you want to use to access the RMI 
# registry/R server.
# It is recommended to use the IP address instead of a name.
# By default, the hostname is extracted form the specified RMI address 
# (script parameter). Nevertheless there are some reasons to set it explicitly
# here, e.g.:
#  - For SSH tunnel connections '127.0.0.1' (localhost) is sufficient;
#    recommend, if no other network connections are used; and required if the 
#    public IP address is blocked by the firewall.
#  - To make sure that the correct IP address is set even the hostname is 
#    used in the RMI address.
##
# Usage:
#     S_HOSTADDRESS=<ip or hostname>
#     S_REGISTRYPORT=<port of rmi-registry>
# Example:
#     S_HOSTADDRESS=192.168.1.80
S_HOSTADDRESS=127.0.0.1/8
S_REGISTRYPORT=1111


###############################################################################
# Add additional java options here
##
# Example:
#     JAVA_OPTS="-server -Dde.walware.rj.rmi.disableSocketFactory=true"
JAVA_OPTS="-server"
JAVA_OPTS_LIB=


###############################################################################
# CONFIG - AUTHENTICATION METHODS #############################################
###############################################################################
# You must specify the method (exactly one) to use for authentication when 
# clients wants to connect to the R.
# There are several methods available. The methods provided by default
# are described in the following subsections. To change the method uncomment
# the lines of method you want to use and comment the previous AUTH definition.
##
# General usage:
#     RJ parm:     -auth=<method-id>[:<config>]
#                  -auth=<classname>[:<config>]
# 
# <config> option depends on the selected method

###############################################################################
# Authentication method: disabled / 'none'
# 
# Disables authentication. Anybody can connect to R. Use it only if you are in 
# a secure environment! All users can connect to R get full user rights!
##
# General usage:
#     RJ param:    -auth=none
# 
# Script Usage:
#     AUTH=none

#AUTH=none

###############################################################################
# Authentication method: password file / 'name-pass'
# 
# Authentication using loginname-password combination.
##
# General usage:
#     RJ Param:    -auth=name-pass:file=<passwordfile>
# 
# Script usage:
#     AUTH=name-pass:file
#     AUTH_PW_FILE=~/.RJServer/logins
# 
# <passwordfile> is the path to a property file with
#     pairs of loginname and password, for example:
#         myname=mypassword
#     Make sure that only authorized users can read this file!

# AUTH=name-pass:file
# AUTH_PW_FILE=~/.RJServer/logins

###############################################################################
# Authentication method: local user account / 'local-shaj'
# 
# Authentication using your local loginname-password combination (PAM).
# The 'Shaj' library is required for this method. It is provided in sources and
# binary form for several platforms. If no binary match your environment, you
# have to build it for your system. You find the files inside the folder
#     shaj
# of this remotetools package.
##
# General Usage:
#     RJ Param:    -auth=local-shaj
#     Java Prop:   java.library.path=<folder of library>
# 
# Script Usage:
#     AUTH=local-shaj
#     AUTH_SHAJ_LD_DIR=<folder of library>
# Example:
#     AUTH=local-shaj
#     AUTH_SHAJ_LD_DIR="$RJS_HOME/shaj/linux-x64"

# AUTH=local-shaj
# AUTH_SHAJ_LD_DIR="$RJS_HOME/shaj/<platform>"

###############################################################################
# Authentication method: ssh / 'fx' (exchange over file)
# 
# Authentication for automatic startup (via ssh by StatET).
##
# General Usage:
#     RJ Param:    -auth=fx:file=<keyfile>
# 
# Script Usage:
#     AUTH=fx:file
#     AUTH_FX_FILE=<keyfile>
#     AUTH_FX_USER=$USER
#     AUTH_FX_MASK=600
# 
# <keyfile> a (empty). The fs permission to edit the files represents the
#     right to login into R, so:
#     The file must be writable for authorized user only!
#     If the script setup is used, the files is created automatically and
#     the permission is set according to AUTH_FX_USER and AUTH_FX_MASK

AUTH=fx:file
AUTH_FX_FILE="$RJS_WORK/session-$S_NAME.lock"
AUTH_FX_USER=$USER
AUTH_FX_MASK=600


###############################################################################
# SCRIPT - STARTUP SERVER #####################################################
###############################################################################
# Usually you don't have to edit the lines below

#$JAVA_HOME/bin/rmiregistry &

mkdir -p "$RJS_WORK"

## Final RMI address
if [ -n "$S_HOSTADDRESS" ]
then
	S_ADDRESS="//$S_HOSTADDRESS"
	if [ -n "$S_REGISTRYPORT" ]
	then
		S_ADDRESS="$S_ADDRESS:$S_REGISTRYPORT"
	fi
	S_ADDRESS="$S_ADDRESS/$S_NAME"
else
	S_ADDRESS="//$S_NAME"
fi

## Finish auth configuration
if [ "$FORCE_AUTH" ]
then
	AUTH="$FORCE_AUTH"
fi

if [ "$AUTH" = "name-pass:file" ]
then
	AUTH="name-pass:file=$AUTH_PW_FILE"
fi

if [ "$AUTH" = "local-shaj" ]
then
	AUTH=local-shaj
	JAVA_OPTS_LIB="$JAVA_OPTS_LIB:$AUTH_SHAJ_LD_DIR"
	JAVA_CP="$JAVA_CP:$RJS_HOME/shaj/auth.jar"
fi

if [ "$AUTH" = "fx:file" ]
then
	AUTH="fx:file=$AUTH_FX_FILE"
	AUTH_FX_FOLDER=`dirname "$AUTH_FX_FILE"`
	mkdir -p "$AUTH_FX_FOLDER"
	echo "00000000" > "$AUTH_FX_FILE"
	if [ $AUTH_FX_USER ]
	then
		chown $AUTH_FX_USER "$AUTH_FX_FILE"
	fi
	if [ $AUTH_FX_MASK ]
	then
		chmod $AUTH_FX_MASK "$AUTH_FX_FILE"
	fi
fi

OPTS="-auth=$AUTH"
if [ $DEBUG ]
then
	OPTS="$OPTS -verbose"
fi

## Java config
if [ $DEV ]
then
	JAVA_CP="$RJS_HOME/../de.walware.rj.server/bin:$RJS_HOME/../de.walware.rj.data/bin:$RJS_HOME/bin:$RJS_HOME/binShaj"
	RMI_BASE="file://$RJS_HOME/../de.walware.rj.server/bin"
	RJAVA_CP=
else
	JAVA_CP="$RJS_HOME/de.walware.rj.server.jar:$RJS_HOME/de.walware.rj.data.jar:$JAVA_CP"
	RMI_BASE="file://$RJS_HOME/de.walware.rj.server.jar"
	RJAVA_CP=
fi

JAVA_OPTS="$JAVA_OPTS -Djava.security.policy=$RJS_HOME/security.policy -Djava.rmi.server.codebase=$RMI_BASE"
if [ -n "$S_HOSTADDRESS" ]
then
	JAVA_OPTS="$JAVA_OPTS -Djava.rmi.server.hostname=$S_HOSTADDRESS"
fi
if [ -n "$JAVA_OPTS_LIB" ]
then
	JAVA_OPTS="$JAVA_OPTS -Djava.library.path=$JAVA_OPTS_LIB"
fi
if [ -n "$RJAVA_CP" ]
then
	JAVA_OPTS="$JAVA_OPTS -Drjava.class.path=$RJAVA_CP"
fi

## Other environment
PATH=$R_HOME/bin:$PATH
LD_LIBRARY_PATH=$R_HOME/lib

export PATH
export LD_LIBRARY_PATH
export JAVA_HOME
export R_HOME
export R_LIBS
export R_LIBS_USER
export R_LIBS_SITE
export R_DOC_DIR
export R_SHARE_DIR
export R_INCLUDE_DIR
export LC_ALL

cd "$WD"

START_EXEC="$JAVA_HOME/bin/java -cp $JAVA_CP $JAVA_OPTS de.walware.rj.server.RMIServerControl start $S_ADDRESS $OPTS"
#echo $START_EXEC

if [ $DEBUG ]
then
	echo S_HOSTADDRESS = $S_HOSTADDRESS
	echo S_REGISTRYPORT = $S_REGISTRY_PORT
	echo PATH = $PATH
	echo LD_LIBRARY_PATH = $LD_LIBRARY_PATH
	echo R_HOME = $R_HOME
	echo JAVA_HOME = $JAVA_HOME
	echo CLASSPATH = $JAVA_CP
	echo JAVA_OPTIONS = $JAVA_OPTS
	echo AUTH = $AUTH
	
	# Start server directly
	$START_EXEC
	START_EXIT=$?
	START_PID=$!
	exit $START_EXIT
else
	# First check if running or dead server is already bound
	CLEAN_EXEC="$JAVA_HOME/bin/java -cp $JAVA_CP $JAVA_OPTS de.walware.rj.server.RMIServerControl clean $S_ADDRESS"
	$CLEAN_EXEC
	CLEAN_EXIT=$?
	if [ $CLEAN_EXIT -ne 0 ]
	then
		echo "Check and cleanup of old server failed (CODE=$CLEAN_EXIT), cancelling startup."
		exit $CLEAN_EXIT
	fi
	
	# Start server detached
	nohup $START_EXEC > "$RJS_WORK/session-$S_NAME.out" 2>&1 < /dev/null &
	START_EXIT=$?
	START_PID=$!
	if [ $START_EXIT -eq 0 ]
	then
		echo "Started server in background (PID=$START_PID)."
		exit 0
	else
		echo "Startup failed"
		exit $START_EXIT
	fi
fi
