def testcase(fn, text, teardown=None, *teardown_args, **teardown_kwargs):
    def inner(*args, **kwargs):
        try:
            print("👉 Running %s ..." % text)
            fn(*args, **kwargs)
            print('📭 Result: %s ︎✅ ' % text)
        except Exception as err:
            print('📭 Result: %s ❌ - %s ︎' % (text, err))
            raise err
        finally:
            if teardown:
                teardown(*teardown_args, **teardown_kwargs)
    return inner
