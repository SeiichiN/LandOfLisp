;; playwav.lisp
;; wavファイルだって鳴らす
;; https://www.slideshare.net/t-sin/sounds-like-common-lisp
;; Sounds Like Common Lisp - ゼロからはじめるサウンドプログラミング

(fret ((find-chunk (name chunks)
                   (find name chunks :test #'string=
                         :key (lambda (c) (getf c :chunk-id))))
       (16bit->float (lsb msb)
                     (float (if (= (logand msb #x80) #x80)
                                (- (- (/ (lognot (+ (ash (logand msb
                                                                 #x7f) 8) lsb))
                                         (expt 2 15)))
                                   1)
                                (/ (+ (ash msb 8) lsb) (expt 2
                                                             15))))))
      (let* ((wav (wav:read-wav-file "ev.wav"))
             (data (getf (find-chunk "data" wav) :chunk-data))
             (buf (make-array (/ (length data) 2))))
        (loop :for i :from 0 :below (length data) :by 4
           :do (setf (aref buf (/ i 2))
                     (16bit->float (aref data (+ i 0))
                                   (aref data (+ i 1)))
                     (aref buf (1+ (/ i 2)))
                     (16bit->float (aref data (+ i 2))
                                   (aref data (+ i 3)))))
        (portaudio:with-audio
            (portaudio:with-default-audio-stream
                (s 0 2 :sample-format :float :sample-rate 44100.0D0
                   :frames-per-buffer (/ (length buf) 2))
              (loop (portaudio:write-stream s buf))))))

;; 修正時刻： Thu Jun  4 08:09:10 2020
