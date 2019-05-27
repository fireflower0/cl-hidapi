# cl-hidapi

## 前準備

libudev用の開発パッケージをインストール

```
sudo apt install libudev-dev libusb-1.0-0-dev libfox-1.6-dev
```

hidapiのダウンロード

```
git clone https://github.com/signal11/hidapi.git
```

hidapiのインストール

```
./bootstrap
./configure
make
sudo make install
```

Common Lisp ライブラリインストール

```
qlot install
```
