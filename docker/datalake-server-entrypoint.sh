#!/usr/bin/env bash


if [ ! -x "${PSQL}" ]; then
    echo "Cannot find psql command: ${PSQL}"
    exit 1
fi

if [ ! -x "${DLSERVER}" ]; then
    echo "Cannot find datalake-server command: ${DLSERVER}"
    exit 1
fi

if [ ! -x "${YQ}" ]; then
    echo "Cannot find yq command: ${YQ}"
    exit 1
fi

