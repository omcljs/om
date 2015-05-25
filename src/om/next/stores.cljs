(ns om.next.stores)

(deftype LocalStore [])

(deftype RemoteStore [fetch local-keys])