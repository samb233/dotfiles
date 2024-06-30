;;; elisp/dirvish-video-mediainfo-enhance.el -*- lexical-binding: t; -*-

(setq dirvish-media--info
      "General;(Full-name . \"\"%FileName%\"\")(Format . \"\"%Format%\"\")(File-size . \"\"%FileSize/String1%\"\")(Duration . \"\"%Duration/String3%\"\")\nImage;(Width . \"\"%Width/String%\"\")(Height . \"\"%Height/String%\"\")(Bit-depth . \"\"%BitDepth/String%\"\")(Color-space . \"\"%ColorSpace%\"\")(Chroma-subsampling . \"\"%ChromaSubsampling%\"\")(Compression-mode . \"\"%Compression_Mode/String%\"\")\nVideo;(Resolution . \"\"%Width% x %Height%\"\")(Video-codec . \"\"%Format%\"\")(Framerate . \"\"%FrameRate%\"\")(Video-bitrate . \"\"%BitRate/String%\"\")\nAudio;(Audio-codec . \"\"%Format%\"\")(Audio-bitrate . \"\"%BitRate/String%\"\")(Audio-sampling-rate . \"\"%SamplingRate/String%\"\")(Audio-channels . \"\"%ChannelLayout%\"\")")

(defun my-dirvish-format-mediainfo-value(old new)
  (if (> (length new) 15)
      (format "%s|%s" old new)
    (let* ((snum (- 15 (length new)))
           (space (make-string snum ? )))
      (if (eq (length old) 0)
          (format "%s%s" new space)
        (format "%s|%s%s" old new space)))))

(defun my-dirvish-format-mediainfo (minfo)
  (let((codec "")
       (bitrate "")
       (samprate "")
       (channel "")
       (count 1))
    (catch 'break
      (dolist (inf minfo)
        (let ((name (car inf))
              (value (cdr inf)))
          (cond
           ((eq name 'Audio-codec)
            (setf codec (my-dirvish-format-mediainfo-value codec (format "#%d: %s" count value)))
            (setf count (+ count 1)))
           ((eq name 'Audio-bitrate)
            (setf bitrate (my-dirvish-format-mediainfo-value bitrate value)))
           ((eq name 'Audio-sampling-rate)
            (setf samprate (my-dirvish-format-mediainfo-value samprate value)))
           ((eq name 'Audio-channels)
            (setf channel (my-dirvish-format-mediainfo-value channel value)))))))
    (setf (alist-get 'Audio-codec minfo) codec)
    (setf (alist-get 'Audio-bitrate minfo) bitrate)
    (setf (alist-get 'Audio-sampling-rate minfo) samprate)
    (setf (alist-get 'Audio-channels minfo) channel)
    minfo))

(advice-add 'dirvish-media--metadata-from-mediainfo :filter-return #'my-dirvish-format-mediainfo)

(provide 'dirvish-video-mediainfo-enhance)
;;; dirvish-video-mediainfo-enhance.el ends here
