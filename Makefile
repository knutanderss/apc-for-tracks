app = apc

# Paths
pngFile = ./icon.png
contents = ./${app}.app/Contents
macos = ${contents}/MacOS
exe = ${macos}/${app}-exe
resources = ./${app}.app/Contents/Resources
iconset = ${resources}/${app}.icns

# https://github.com/auriamg/macdylibbundler
dylibbundler = ~/git/macdylibbundler/dylibbundler

${exe} : src app
		stack build --copy-bins --local-bin-path ${macos}
		${dylibbundler} -od -b -x ${exe} -d ${contents}/libs/

${iconset} : ${pngFile}
		mkdir MyIcon.iconset
		sips -z 16 16     ${pngFile} --out MyIcon.iconset/icon_16x16.png
		sips -z 32 32     ${pngFile} --out MyIcon.iconset/icon_16x16@2x.png
		sips -z 32 32     ${pngFile} --out MyIcon.iconset/icon_32x32.png
		sips -z 64 64     ${pngFile} --out MyIcon.iconset/icon_32x32@2x.png
		sips -z 128 128   ${pngFile} --out MyIcon.iconset/icon_128x128.png
		sips -z 256 256   ${pngFile} --out MyIcon.iconset/icon_128x128@2x.png
		sips -z 256 256   ${pngFile} --out MyIcon.iconset/icon_256x256.png
		sips -z 512 512   ${pngFile} --out MyIcon.iconset/icon_256x256@2x.png
		sips -z 512 512   ${pngFile} --out MyIcon.iconset/icon_512x512.png
		cp ${pngFile} MyIcon.iconset/icon_512x512@2x.png
		iconutil -c icns MyIcon.iconset
		rm -R MyIcon.iconset
		mv ./MyIcon.icns ${iconset}

.PHONY: clean

clean:
		stack clean
		rm ${exe}
