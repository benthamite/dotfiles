;;; simple-extras-test.el --- Tests for simple-extras -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'simple-extras)

(ert-deftest simple-extras-slugify-handles-unicode-punctuation ()
  "Ensure `simple-extras-slugify' removes Unicode punctuation and symbols."
  (should (equal (simple-extras-slugify "【キャリアガイド】第4部：良いことをしたい")
                 "キャリアガイド-第4部-良いことをしたい"))
  (should (equal (simple-extras-slugify "لماذا نحتاج إلى الأخلاق؟")
                 "لماذا-نحتاج-إلى-الأخلاق"))
  (should (equal (simple-extras-slugify "Café & croissants: a “guide”")
                 "cafe-croissants-a-guide")))

(provide 'simple-extras-test)
;;; simple-extras-test.el ends here
