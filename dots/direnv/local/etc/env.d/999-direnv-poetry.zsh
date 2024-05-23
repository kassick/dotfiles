direnv-poetry-init() {
    if [[ ! -f "pyproject.toml" ]]; then
        poetry init --python=^`python --version|awk '{ print $2 ; }'|awk -F "." '{ print $1 "."  $2 ; }'`
        if [[ $? != 0 ]]; then
            echo Aborting
            return
        fi
    fi

    cat >.envrc <<EOF
layout poetry
EOF
    direnv allow
}

direnv-poetry-init-with-venv() {
    if [[ ! -f "pyproject.toml" ]]; then
        poetry init --python=^`python --version|awk '{ print $2 ; }'|awk -F "." '{ print $1 "."  $2 ; }'`
        if [[ $? != 0 ]]; then
            echo Aborting
            return
        fi
    fi

    venv_target="${1:-.venv}"

    if [[ -f "${venv_target}/bin/python" ]]; then
        echo "Using exiting venv at ${venv_target}"
    else
        python -m venv "${venv_target}"
    fi

    poetry env use "${venv_target}/bin/python"

    cat >.envrc <<EOF
layout poetry
EOF
    direnv allow
}
