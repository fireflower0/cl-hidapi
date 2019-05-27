(defsystem "cl-hidapi"
    :class :package-inferred-system
    :version "0.1.0"
    :author "fireflower0"
    :license "MIT"
    :depends-on ("cffi"
                 "cl-hidapi/lib-hidapi"
                 "cl-hidapi/examples/hidapi-app"))
