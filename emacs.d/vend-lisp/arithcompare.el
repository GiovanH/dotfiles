; backport emacs ebb99847285bca912e04f79dd3d9dcc84769ccf6
(if (not (fboundp 'c=)) (defalias 'c= (symbol-function '=)))
(defun = (num1 &rest nums) (every (lambda (num2) (c= num1 num2)) nums))

(if (not (fboundp 'c<=)) (defalias 'c<= (symbol-function '<=)))
(defun <= (&rest nums) (every #'c<= nums (rest nums)))

(if (not (fboundp 'c<)) (defalias 'c< (symbol-function '<)))
(defun < (&rest nums) (every #'c< nums (rest nums)))

(if (not (fboundp 'c>=)) (defalias 'c>= (symbol-function '>=)))
(defun >= (&rest nums) (every #'c>= nums (rest nums)))

(if (not (fboundp 'c>)) (defalias 'c> (symbol-function '>)))
(defun > (&rest nums) (every #'c> nums (rest nums)))

(ert-deftest data-tests-= ()
  ;(should-error (=))
  (should (= 1))
  (should (= 2 2))
  (should (= 9 9 9 9 9 9 9 9 9))
  (should-not (apply #'= '(3 8 3)))
  (should-error (= 9 9 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (= 9 8 'foo)))

(ert-deftest data-tests-< ()
  ;(should-error (<))
  (should (< 1))
  (should (< 2 3))
  (should (< -6 -1 0 2 3 4 8 9 999))
  (should-not (apply #'< '(3 8 3)))
  (should-error (< 9 10 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (< 9 8 'foo)))

(ert-deftest data-tests-> ()
  ;(should-error (>))
  (should (> 1))
  (should (> 3 2))
  (should (> 6 1 0 -2 -3 -4 -8 -9 -999))
  (should-not (apply #'> '(3 8 3)))
  (should-error (> 9 8 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (> 8 9 'foo)))

(ert-deftest data-tests-<= ()
  ;(should-error (<=))
  (should (<= 1))
  (should (<= 2 3))
  (should (<= -6 -1 -1 0 0 0 2 3 4 8 999))
  (should-not (apply #'<= '(3 8 3 3)))
  (should-error (<= 9 10 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (<= 9 8 'foo)))

(ert-deftest data-tests->= ()
  ;(should-error (>=))
  (should (>= 1))
  (should (>= 3 2))
  (should (>= 666 1 0 0 -2 -3 -3 -3 -4 -8 -8 -9 -999))
  (should-not (apply #'>= '(3 8 3)))
  (should-error (>= 9 8 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (>= 8 9 'foo)))
