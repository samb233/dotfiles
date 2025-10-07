;;; elisp/dirvish-video-mediainfo-enhance.el -*- lexical-binding: t; -*-

;; (dirvish-define-preview video-mtn-2 (file ext preview-window)
;;   "Preview video files on MS-Windows.
;; Require: `mtn' (executable)"
;;   :require ("echo")
;;   (when (member ext dirvish-video-exts)
;;     (let* ((width (dirvish-media--img-size preview-window))
;;            (height (dirvish-media--img-size preview-window 'height))
;;            (cache (dirvish--img-thumb-name file width ".jpg"))
;;            (path (dirvish--get-parent-path cache)))
;;       (if (file-exists-p cache)
;;           `(shell . ("echo" ,file))
;;         `(shell . ("echo" "not exist cache" ,cache "file: " ,file))))))
;; ;; (delq "video-mtn" 'dirvish-preview-dispatchers)
;; (add-to-list 'dirvish-preview-dispatchers 'video-mtn-2)
;; ;; (setq 'dirvish-preview-dispatchers '(video-mtn-2 image gif audio epub archive font pdf))

(dirvish-define-preview video-mtn-test (file ext preview-window)
  "Preview video files on MS-Windows.
Require: `mtn' (executable)"
  :require (dirvish-mtn-program)
  (when (member ext dirvish-video-exts)
    (let* ((width (dirvish-media--img-size preview-window))
           (height (dirvish-media--img-size preview-window 'height))
           (cache (dirvish--img-thumb-name file width ".jpg"))
           (path (dirvish--get-parent-path cache))
           (cache-base (file-name-base cache)))
      (if (file-exists-p cache)
          `(img . ,(create-image cache nil nil :max-width width :max-height height))
        `(cache . (,dirvish-mtn-program "-P" "-i" "-c" "1" "-r" "1" "-O" ,path ,file "-x"
                                        ,cache-base "-o" ".jpg" "-w"
                                        ,(number-to-string width)))))))
(setq dirvish-preview-dispatchers '(video-mtn-test image gif audio epub archive font pdf))
;; (add-to-list 'dirvish-preview-dispatchers 'video-mtn-test)

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
