#!/bin/bash -e

doc_root="exports"
doc_dirs=(Appendix_2_Service_Description
Appendix_4_Evidence_of_time
Appendix_6_Digital_Signature_Documentation
ZZTestDocument)

tmp_dir=$1
if [ -n "$tmp_dir" ]; then

	printf "Pre-processing files into '$tmp_dir'...\t"
	if [ -d "$tmp_dir" ]; then
		rm -r "$tmp_dir"
	fi
	mkdir "$tmp_dir"
	mkdir "$tmp_dir/images"

	cp $doc_root/*.html $tmp_dir/

	cd "$doc_root"
	for dir in ${doc_dirs[@]}; do
		cd "$dir"
		for html in ./*.html; do
			# Change all 'src' attributes in the HTML to prepend directory name
			sed -e "s/src=['\"]images\/\([A-z0-9]*\).png['\"]/src=\"images\/${dir}_\1.png\"/g" $html > "../../$tmp_dir/$html"
		done
		# Use 'mcp' to bulk copy images with new file name prefix
		mcp "images/*.png" "../../$tmp_dir/images/${dir}_#1.png"
		cd ..
	done
	cd ..
	echo "Done."

	# Make images smaller
	printf "Optimising files in '$tmp_dir/images/'...\t"
	optipng $tmp_dir/images/* 2> /dev/null
	echo "Done."

else

	printf "Error:\tNeed a directory as argument\n"

fi
