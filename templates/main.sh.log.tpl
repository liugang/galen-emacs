#!/usr/bin/env bash

# Time-stamp: <2011-03-18 18:02:22 Friday by taoshanwen>

# @file (>>>FILE<<<)
# @version 1.0
# @author ahei

readonly PROGRAM_NAME="(>>>FILE<<<)"
readonly PROGRAM_VERSION="1.0.0"

home=`dirname "$0"`
home=`cd "$home" && pwd`

. "$home"/common.sh

usage()
{
    local code=1
    local redirect
    
    if [ $# -gt 0 ]; then
        code="$1"
    fi

    if [ "$code" != 0 ]; then
        redirect="1>&2"
    fi

    eval cat "$redirect" << EOF
usage: ${PROGRAM_NAME} [OPTIONS]

Options:
    -D [DEBUG_LEVEL]
        Set debug level, can be: TRACE, DEBUG, INFO, WARN, ERROR, FATAL, OFF.
    -v  Output version info.
    -h  Output this help.
EOF

    exit "$code"
}

version()
{
    echoo "${PROGRAM_NAME} ${PROGRAM_VERSION}"
    exit
}

isExecute=1
isStop=1

optInd=1

while getopts ":D:hv" OPT; do
    case "$OPT" in
        D)
            level="$OPTARG"
            let optInd+=2
            ;;

        v)
            version
            ;;

        h)
            usage 0
            ;;

        :)
            case "${OPTARG}" in
                D)
                    level="DEBUG"
                    let optInd++
                    ;;

                ?)
                    echoe "Option \`-${OPTARG}' need argument.\n"
                    usage
            esac
            ;;

        ?)
            echoe "Invalid option \`-${OPTARG}'.\n"
            usage
            ;;
    esac
done

shift $((optInd - 1))

LOG4SH_CONFIGURATION="$home/../conf/log4sh.properties" . "$home/../shell/log4sh"

if [ -n "$level" ]; then
    level=`tr '[a-z]' '[A-Z]' <<< "$level"`

    appender_setLevel "FILE" "$level"
    appender_setLevel "CONSOLE" "$level"
fi

ecArgs="$isExecute $isStop $mailLists"

(>>>POINT<<<)
