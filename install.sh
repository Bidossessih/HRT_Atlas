docker load -i HTAtlas_v1.tar

echo HTAtlas_v1.tar unzipped

docker run --restart always -p 80:80 ht_atlas_v1.0:latest &

echo Done