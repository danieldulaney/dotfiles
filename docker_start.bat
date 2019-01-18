set user=%USERNAME%
set ssh_keyfile=C:\Program Files\PuTTY\Petes Laptop Key

docker build -t workspace %~dp0 || exit /B
docker run --rm --mount "type=bind,source=%ssh_keyfile%,target=/home/%user%/.ssh/id_rsa,readonly" -it workspace

